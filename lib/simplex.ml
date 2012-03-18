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

(* Uni-directional shared-memory simplex channel. There is no attempt
 * to coordinate both sides (see the various metadata transports for that) *)

type 'a ring

module Raw = struct
  type mode = Recv | Send
  type extent = int * int (* offset * length, -1 if error *)
  type buf = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  (* Given a shared memory file descriptor (usually obtained by shm_open(2),
   * and which end of the transport this is, and the size of the shared area,
   * establish a shared memory ring.
   *)
  external attach: Shm.shm_descr -> mode -> int -> 'a ring = "ocaml_simplex_alloc"
  external free_space: 'a ring -> bool = "ocaml_any_shared_space"
  external alloc: 'a ring -> int -> extent = "ocaml_alloc_shared_tx_space"
  external buf_of_extent: 'a ring -> int -> int -> buf = "ocaml_alloc_shared_extent"
  external release: 'a ring -> int -> int -> unit = "ocaml_release_shared_extent"
  external ba_is_member: 'a ring -> buf -> bool = "ocaml_ba_is_member"
  let valid_extent = function
  |(-1,-1) -> false
  |_ -> true
end

type 'a extent = 'a ring * Raw.extent
type buf = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let attach_tx fd nr_bytes  =
  Unix.ftruncate (Shm.unix_descr_of_shm fd) nr_bytes;
  Raw.attach fd Raw.Send nr_bytes

let attach_rx fd nr_bytes =
  Raw.attach fd Raw.Recv nr_bytes

let has_free_space ring =
  Raw.free_space ring

let alloc ring len =
  let extent = Raw.alloc ring len in
  match Raw.valid_extent extent with
  |true -> Some (ring, extent)
  |false -> None

let release (ring, (off,len)) =
  Raw.release ring off len

let buffer (ring, (off,len)) =
  Raw.buf_of_extent ring off len

let length (_,(off,len)) = len
let offset (_,(off,len)) = off

let extent_is_member ring (ring',_) =
  ring == ring'

let buf_is_member ring buf =
  Raw.ba_is_member ring buf

type op =
|Send of int * int
|Free of int * int

let to_send_op (_,(off,len)) = Send (off,len)
let to_free_op (_,(off,len)) = Free (off,len)
let on_op ~rx ~tx ~send ~free =
  function
  |Send (off,len) -> send (rx, (off,len))
  |Free (off,len) -> free (tx, (off,len))

