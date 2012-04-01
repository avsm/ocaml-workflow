/*
 * Copyright (c) 2012 Steven Smith <Steven.Smith@cl.cam.ac.uk>
 * Copyright (c) 2012 Chris Smowton <Chris.Smowton@cl.cam.ac.uk>
 * Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

/* We have a shared-memory region and an malloc-style interface for
 * allocating from it, and also a couple of coordination pipes.
 * Messages are sent by allocating a chunk of the shared region and
 * then sending an extent through the pipe.  Once the receiver is finished
 * with the message, they send another extent back through the other pipe
 * signifying that they're done.
 */

#define NDEBUG
#define _GNU_SOURCE
#include <sys/poll.h>
#include <sys/time.h>
#include <sys/uio.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <assert.h>
#include <err.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <stddef.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <math.h>

#include <sys/select.h>
#include <sys/socket.h>
#include <sys/uio.h>
#include <assert.h>
#include <errno.h>

/* The buffer that an application receives to read and write into */
struct fable_buf {

  struct iovec* bufs;
  int nbufs;
  int written;

} __attribute__((packed));

#define PAGE_ORDER 12
#define CACHE_LINE_SIZE 64

#define PAGE_SIZE 4096
#define EXTENT_BUFFER_SIZE 4096

#define ALLOC_FAILED ((unsigned)-1)

#ifdef NDEBUG
#define DBG(x) do {} while (0)
#else
#define DBG(x) do { x; } while (0)
#endif

//#define DBGPRINT printf
#define DBGPRINT(...) do {} while (0)

/* Parameters for a simplex connection. Currently just tracks if the
 * buffer is the sender or receiver end. */
typedef struct {
  int is_send;
} simplex_id_t;

#define SIMPLEX_SEND ((simplex_id_t){1})
#define SIMPLEX_RECV ((simplex_id_t){0})

/* A single simplex shared buffer, of configurable page size */
struct shmem_simplex {
  simplex_id_t simpl_id;
  void *ring;
  int ring_pages;
  struct alloc_node *first_alloc, *next_free_alloc, *last_freed_node;
#ifndef NDEBUG
  int nr_alloc_nodes;
#endif
};

/* Our allocation structure is a simple linked list.  That's pretty
 * stupid, *except* that the allocation pattern is almost always a
 * very simple queue, so it becomes very simple.  i.e. we release
 * stuff in a FIFO order wrt allocations, so we effectively just have
 * one allocated region which loops around the shared area, which
 * makes the linked list very short and everything is very easy. 
 */
struct alloc_node {
  struct alloc_node *next, *prev;
  int is_free;
  unsigned long start;
  unsigned long end;
};

#ifndef NDEBUG
static void
sanity_check(const struct shmem_simplex *sp)
{
  const struct alloc_node *cursor;
  int found_nf = 0, found_lf = 0;
  int n = 0;
  assert(sp->first_alloc);
  assert(sp->first_alloc->start == 0);
  for (cursor = sp->first_alloc; cursor; cursor = cursor->next) {
    n++;
    if (cursor == sp->first_alloc)
      assert(!cursor->prev);
    else
      assert(cursor->prev);
    if (cursor->next)
      assert(cursor->next->prev == cursor);
    if (cursor->prev)
      assert(cursor->start == cursor->prev->end);
    if (cursor->prev)
      assert(cursor->is_free == !cursor->prev->is_free);
    if (!cursor->next)
      assert(cursor->end == (sp->ring_pages * PAGE_SIZE));
    if (cursor == sp->next_free_alloc) {
      assert(!found_nf);
      found_nf = 1;
    }
    if (cursor == sp->last_freed_node) {
      assert(!found_lf);
      found_lf = 1;
    }
    assert(cursor->start < cursor->end);
  }
  if (!found_nf)
    assert(!sp->next_free_alloc);
  else
    assert(sp->next_free_alloc->is_free);
  if (!found_lf)
    assert(!sp->last_freed_node);
  else
    assert(sp->last_freed_node->is_free);
  assert(n == sp->nr_alloc_nodes);
}
#else
static void
sanity_check(const struct shmem_simplex *sp)
{
}
#endif

static unsigned
any_shared_space(struct shmem_simplex *sp)
{

  /* As its name suggests, if next_free_alloc is set, then it points to a free
   * area. We can write at least 1 byte, so we're writable. */
  if(sp->next_free_alloc)
    return 1;

  /* Search the linked list. Don't keep next_free_alloc as it'd make the
   * alloc_shared take a silly decision when we stop at a 1-byte hole.  */

  struct alloc_node *n;
  for (n = sp->first_alloc; n && (!n->is_free); n = n->next) {};

  return (n != 0);
}

static unsigned
alloc_shared_space(struct shmem_simplex *sp, unsigned *size_out)
{
  unsigned size = *size_out;
  unsigned res;

  sanity_check(sp);

  /* Common case */
  if (sp->next_free_alloc &&
      sp->next_free_alloc->end >= size + sp->next_free_alloc->start &&
      sp->next_free_alloc->prev) {
  allocate_next_free:
    assert(!sp->next_free_alloc->prev->is_free);
    assert(sp->next_free_alloc->is_free);
    res = sp->next_free_alloc->start;
    sp->next_free_alloc->start += size;
    sp->next_free_alloc->prev->end += size;
    if (sp->next_free_alloc->start == sp->next_free_alloc->end) {
      if (sp->next_free_alloc->next) {
        assert(!sp->next_free_alloc->next->is_free);
        sp->next_free_alloc->prev->next = sp->next_free_alloc->next->next;
        sp->next_free_alloc->prev->end = sp->next_free_alloc->next->end;
        if (sp->next_free_alloc->next->next) {
          assert(sp->next_free_alloc->next->next->is_free);
          sp->next_free_alloc->next->next->prev = sp->next_free_alloc->prev;
        }
        struct alloc_node *p = sp->next_free_alloc->next->next;
        DBG(sp->nr_alloc_nodes--);
        free(sp->next_free_alloc->next);
        if (sp->next_free_alloc->next == sp->last_freed_node)
          sp->last_freed_node = NULL;
        sp->next_free_alloc->next = p;
      } else {
        if (sp->next_free_alloc->prev)
          sp->next_free_alloc->prev->next = NULL;
      }
      if (sp->first_alloc == sp->next_free_alloc) {
        assert(sp->next_free_alloc->next);
        assert(!sp->next_free_alloc->prev);
        sp->first_alloc = sp->next_free_alloc->next;
      }
      if (sp->next_free_alloc == sp->last_freed_node)
        sp->last_freed_node = NULL;
      DBG(sp->nr_alloc_nodes--);
      free(sp->next_free_alloc);
      sp->next_free_alloc = NULL;
    }
    sanity_check(sp);
    return res;
  }

  struct alloc_node *best_candidate = 0;
  unsigned int best_size = 0;

  /* Slightly harder case: have to search the linked list */
  for (sp->next_free_alloc = sp->first_alloc;
       sp->next_free_alloc &&
         (!sp->next_free_alloc->is_free || sp->next_free_alloc->end - sp->next_free_alloc->start < size);
       sp->next_free_alloc = sp->next_free_alloc->next) {
           
    /* sp->next_free_alloc isn't enough or doesn't exist, but keep track of the next-best option */
    if(sp->next_free_alloc && sp->next_free_alloc->is_free) {
      unsigned int this_size = sp->next_free_alloc->end - sp->next_free_alloc->start;
      if(this_size > best_size) {
        best_size = this_size;
        best_candidate = sp->next_free_alloc;
      }
    }

  }
  if (!sp->next_free_alloc) {
    /* Shared area has no gaps large enough, but in order to behave like a selectable device we
       must return the next best candidate if there is one. */
    if(best_candidate) {
      sp->next_free_alloc = best_candidate;
      (*size_out) = size = best_size;
    }
    else
      return ALLOC_FAILED;
  }

  struct alloc_node *f = sp->next_free_alloc;
  assert(f->is_free);
  if (!f->prev) {
    /* Allocate the start of the arena. */
    assert(f->start == 0);
    assert(f == sp->first_alloc);
    if (f->end == size) {
      /* We're going to convert next_free_alloc to
       * an in-use node.  This may involve forwards
       * merging. */
      if (f->next) {
        struct alloc_node *t = f->next;
        assert(!t->is_free);
        f->end = t->end;
        f->next = t->next;
        if (f->next)
          f->next->prev = f;
        if (sp->last_freed_node == t)
          sp->last_freed_node = NULL;
        DBG(sp->nr_alloc_nodes--);
        free(t);
      }
      f->is_free = 0;
    } else {
            f = calloc(sizeof(struct alloc_node), 1);
      DBG(sp->nr_alloc_nodes++);
      f->next = sp->first_alloc;
      f->start = 0;
      f->end = size;
      assert(f->next);
      f->next->prev = f;
      f->next->start = size;
      sp->first_alloc = f;
    }
    if (sp->last_freed_node == sp->first_alloc)
      sp->last_freed_node = sp->first_alloc->next;
    if (sp->next_free_alloc == sp->first_alloc)
      sp->next_free_alloc = sp->first_alloc->next;
    sanity_check(sp);
    return 0;
  } else {
    goto allocate_next_free;
  }
}

static void
release_shared_space(struct shmem_simplex *sp, unsigned start, unsigned size)
{
  DBGPRINT("Release [%x,%x) on %p\n", start, start+size, (void *)sp);
  struct alloc_node *lan = sp->last_freed_node;
  assert(start <= sp->ring_pages * PAGE_SIZE);
  assert(start + size <= (sp->ring_pages * PAGE_SIZE));
  assert(size > 0);
  sanity_check(sp);
  if (lan &&
      lan->is_free &&
      lan->end == start) {
    struct alloc_node *next;
  free_from_here:
    next = lan->next;
    assert(next);
    assert(!next->is_free);
    assert(next->start == start);
    assert(next->end >= start + size);
    next->start += size;
    lan->end += size;
    if (next->start == next->end) {
      /* We just closed a hole.  Previously, we had
         LAN->next->X, where LAN is sp->last_freed_node,
         next is some free region, and X is either
         NULL or some allocated region.  next is now
         zero-sized, so we want to remove it and
         convert to LAN->X.  However, LAN and X are
         the same type (i.e. both non-free), so we
         can extend LAN to cover X and remove X as
         well. */
      struct alloc_node *X = next->next;

      if (X) {
        /* Convert LAN->next->X->Y into
           LAN->Y */
        struct alloc_node *Y = X->next;
        assert(X->is_free);
        if (Y) {
          assert(!Y->is_free);
          Y->prev = lan;
        }
        lan->end = X->end;
        lan->next = Y;
        if (X == sp->next_free_alloc)
          sp->next_free_alloc = lan;
        DBG(sp->nr_alloc_nodes--);
        free(X);
      } else {
        /* Just turn LAN->free1->NULL into
           LAN->NULL */
        assert(lan->end == next->start);
        lan->next = NULL;
      }
      if (next == sp->next_free_alloc)
        sp->next_free_alloc = lan;
      DBG(sp->nr_alloc_nodes--);
      free(next);
    }
    sanity_check(sp);
    return;
  }

  /* More tricky case: we're freeing something which doesn't hit
   * the cache. */
  for (lan = sp->first_alloc;
       lan && (lan->end <= start || lan->start > start);
       lan = lan->next)
    ;
  assert(lan); /* Or else we're freeing something outside of the arena */
  assert(!lan->is_free); /* Or we have a double free */
  if (lan->start == start) {
    /* Free out the start of this block. */
    assert(!lan->is_free);
    if (lan->prev) {
      assert(lan->prev->is_free);
      assert(lan->prev->end == start);
      sp->last_freed_node = lan = lan->prev;
      goto free_from_here;
    }
    /* Turn the very start of the arena into a free
     * block */
    assert(lan == sp->first_alloc);
    assert(start == 0);
    if (lan->end == size) {
      /* Easy: just convert the existing node to a
       * free one. */
      lan->is_free = 1;
      if (lan->next && lan->next->is_free) {
        /* First node is now free, and the
           second node already was -> merge
           them. */
        struct alloc_node *t = lan->next;
        lan->end = t->end;
        lan->next = t->next;
        if (lan->next)
          lan->next->prev = lan;
        if (sp->last_freed_node == t)
          sp->last_freed_node = lan;
        if (sp->next_free_alloc == t)
          sp->next_free_alloc = lan;
        DBG(sp->nr_alloc_nodes--);
        free(t);
      }
      sanity_check(sp);
    } else {
      /* Need a new node in the list */
                  lan = calloc(sizeof(*lan), 1);
      lan->is_free = 1;
      lan->end = size;
      sp->first_alloc->start = lan->end;
      sp->first_alloc->prev = lan;
      lan->next = sp->first_alloc;
      sp->first_alloc = lan;
      sp->last_freed_node = sp->first_alloc;
      DBG(sp->nr_alloc_nodes++);
      sanity_check(sp);
    }
    return;
  }
  assert(start < lan->end);
  assert(start + size <= lan->end);
  if (start + size == lan->end) {
    /* Free out the end of this block */
    if (lan->next) {
      assert(lan->next->is_free);
      lan->next->start -= size;
      lan->end -= size;
      assert(lan->end != lan->start);
    } else {
      struct alloc_node *t = calloc(sizeof(*lan), 1);
      t->prev = lan;
      t->is_free = 1;
      t->start = start;
      t->end = start + size;
      lan->next = t;
      lan->end = start;
      DBG(sp->nr_alloc_nodes++);
    }
    if (!sp->next_free_alloc)
      sp->next_free_alloc = lan->next;
    sp->last_freed_node = lan->next;
    sanity_check(sp);
    return;
  }

  /* Okay, this is the tricky case.  We have a single allocated
     node, and we need to convert it into three: an allocated
     node, a free node, and then another allocated node.  How
     tedious. */
  struct alloc_node *a = calloc(sizeof(*a), 1);
  struct alloc_node *b = calloc(sizeof(*b), 1);

  a->next = b;
  a->prev = lan;
  a->is_free = 1;
  a->start = start;
  a->end = start + size;

  b->next = lan->next;
  b->prev = a;
  b->is_free = 0;
  b->start = start + size;
  b->end = lan->end;

  if (lan->next)
    lan->next->prev = b;
  lan->next = a;
  lan->end = start;

  DBG(sp->nr_alloc_nodes += 2);

  if (!sp->next_free_alloc)
    sp->next_free_alloc = a;

  /* And we're done. */
  sanity_check(sp);
}

// OCaml bits from here
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/bigarray.h>
#include <caml/fail.h>
#include <caml/unixsupport.h>

static void
shmem_simplex_init(struct shmem_simplex *sh, int shmem_fd, int mode, size_t nr_bytes)
{
  int seg_pages = nr_bytes / PAGE_SIZE; /* TODO assert page aligned */
  sh->ring = mmap(NULL, PAGE_SIZE * seg_pages, PROT_READ|PROT_WRITE, MAP_SHARED, shmem_fd, 0);
  if (sh->ring == MAP_FAILED)
    uerror("simplex_init", Val_unit);
  sh->ring_pages = seg_pages;
  sh->simpl_id.is_send = mode;
  if (mode==0) { /* RECV */
  } else { /* SEND */
    sh->simpl_id.is_send=1;
    sh->first_alloc = (struct alloc_node*)caml_stat_alloc(sizeof(struct alloc_node));
    memset(sh->first_alloc, 0, sizeof(struct alloc_node));
    sh->first_alloc->is_free = 1;
    sh->first_alloc->end = seg_pages * PAGE_SIZE;
    DBG(sh->nr_alloc_nodes = 1);
    sanity_check(sh);
  }
}

#define Simplex_wrap_val(x) (*((struct shmem_simplex **)(Data_custom_val(x))))
static void
simplex_wrap_finalize(value v_sh)
{
  struct shmem_simplex *sh = Simplex_wrap_val(v_sh);
  if (sh==NULL) {
    fprintf (stderr, "panic: simplex_wrap_finalize already NULL\n");
    _exit(1);
  }
  free(sh);
  /* TODO unmap ring. shmem unlink done at ocaml level */
  Simplex_wrap_val(v_sh) = NULL;
}

value
ocaml_simplex_alloc(value v_fd, value v_mode, value v_nr_bytes)
{
  CAMLparam3(v_fd, v_mode, v_nr_bytes);
  CAMLlocal1(v_sh);
  v_sh = caml_alloc_final(2, simplex_wrap_finalize, 1, 100);
  Simplex_wrap_val(v_sh) = NULL;
  struct shmem_simplex *sh = caml_stat_alloc(sizeof (struct shmem_simplex));
  memset(sh, 0, sizeof (struct shmem_simplex));
  Simplex_wrap_val(v_sh) = sh;
  shmem_simplex_init(sh, Int_val(v_fd), Int_val(v_mode), Int_val(v_nr_bytes));
  CAMLreturn(v_sh);
}

value
ocaml_any_shared_space(value v_ring)
{
  CAMLparam1(v_ring);
  CAMLreturn(Val_int(any_shared_space(Simplex_wrap_val(v_ring))));
}

/* Allocate a bigarray in the transmitter tx space */
value
ocaml_alloc_shared_tx_space(value v_ring, value v_len)
{
  CAMLparam2(v_ring, v_len);
  CAMLlocal1(v_ret);
  struct shmem_simplex *sh = Simplex_wrap_val(v_ring);
  sanity_check(sh);
  unsigned len = Int_val(v_len);
  int offset = 0;
  v_ret = caml_alloc(2, 0);
  if (len < 1) {
     Store_field(v_ret, 0, -1);
     Store_field(v_ret, 1, -1);
     CAMLreturn(v_ret);
  }
  offset = alloc_shared_space(sh, &len);
  if (offset == ALLOC_FAILED) {
    Store_field(v_ret, 0, -1);
    Store_field(v_ret, 1, -1);
    CAMLreturn(v_ret);
  }
  /* offset is ptr into ring, and len will be set to amount
  * that was actually allocated (XXX i think) */
  Store_field(v_ret, 0, Val_int(offset));
  Store_field(v_ret, 1, Val_int(len));
  CAMLreturn(v_ret);
}

/* Given an offset and length, allocate a Bigarray out of the
 * shared space. */
value
ocaml_alloc_shared_extent(value v_ring, value v_off, value v_len)
{
  CAMLparam3(v_ring, v_off, v_len);
  CAMLlocal1(v_arr);
  struct shmem_simplex *sh = Simplex_wrap_val(v_ring);
  int len = Int_val(v_len);
  void *data = sh->ring + Int_val(v_off); /* XXX bounds check */
  v_arr = caml_ba_alloc_dims(CAML_BA_EXTERNAL | CAML_BA_UINT8 | CAML_BA_C_LAYOUT, 1, data, len);
  CAMLreturn(v_arr);
}

value
ocaml_release_shared_extent(value v_ring, value v_off, value v_len)
{
  CAMLparam3(v_ring, v_off, v_len);
  struct shmem_simplex *sh = Simplex_wrap_val(v_ring);
  release_shared_space(sh, Int_val(v_off), Int_val(v_len));
  CAMLreturn(Val_unit);
}

/* Given a Simplex and a bigarray, test if the underlying buffer
 * is a member of the ring memory, or is allocated elsewhere. 
 */
value
ocaml_ba_is_member(value v_ring, value v_ba)
{
  CAMLparam2(v_ring, v_ba);
  struct shmem_simplex *sh = Simplex_wrap_val(v_ring);
  void *dst_data = Caml_ba_data_val(v_ba);
  void *src_data = sh->ring;
  if (dst_data < src_data)
    CAMLreturn(Val_int(0));
  if (dst_data >= (src_data + (sh->ring_pages * PAGE_SIZE)))
    CAMLreturn(Val_int(0));
  CAMLreturn(Val_int(1));
}

