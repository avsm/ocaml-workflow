(*
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
 *)

type 'a ring
type 'a extent
type buf = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

val attach_tx : Shm.shm_descr -> int -> [`tx] ring
val attach_rx : Shm.shm_descr -> int -> [`rx] ring

val has_free_space : [`tx] ring -> bool
val extent_is_member : 'a ring -> 'a extent -> bool
val buf_is_member : 'a ring -> buf -> bool

val alloc   : [`tx] ring -> int -> [`tx] extent option
val release : [`tx] extent -> unit
val buffer  : [<`tx|`rx] extent -> buf
val length  : [<`tx|`rx] extent -> int
val offset  : [<`tx|`rx] extent -> int

(* Metadata operations to free received extents or transmit new extents *)
type op
val to_send_op : 'a extent -> op
val to_free_op : 'a extent -> op
val to_close_op : op
(* Multiplex a received operation to the respective handlers *)
val on_op : 
  rx:([`rx] ring) ->
  tx:([`tx] ring) ->
  send:([`rx] extent -> unit) ->
  free:([`tx] extent -> unit) -> 
  close:([`rx] ring -> unit) -> op -> unit

val debug_op : op -> unit
