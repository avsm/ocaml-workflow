/*
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

#include <stdlib.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <sys/mman.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>

value
ocaml_shm_open(value v_name, value v_rw, value v_creat, value v_excl, value v_trunc)
{
  CAMLparam5(v_name, v_rw, v_creat, v_excl, v_trunc);
  char *path;
  int fd;
  int flags = (Bool_val(v_rw)) ? O_RDWR : O_RDONLY;
  if (Bool_val(v_creat)) flags |= O_CREAT;
  if (Bool_val(v_excl)) flags |= O_EXCL;
  if (Bool_val(v_trunc)) flags |= O_TRUNC;
  path = caml_stat_alloc(caml_string_length(v_name)+1);
  strcpy(path, String_val(v_name)); 
  enter_blocking_section();
  fd = shm_open(path, flags);
  leave_blocking_section();
  caml_stat_free(path);
  if (fd == -1)
    uerror("shm_open", v_name);
  CAMLreturn(Val_int(fd));
}

value
ocaml_shm_unlink(value v_name)
{
  CAMLparam1(v_name);
  char *path;
  int ret;
  path = caml_stat_alloc(caml_string_length(v_name)+1);
  strcpy(path, String_val(v_name)); 
  enter_blocking_section();
  ret = shm_unlink(path);
  leave_blocking_section();
  caml_stat_free(path);
  if (ret == -1)
    uerror("shm_unlink", v_name);
  CAMLreturn(Val_unit);
}
