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

open Lwt
open Printf

(* Buffers are all wrapped in Bigarrays *)
type ba = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type 'a tx = {
  tx_send: 'a -> bool Lwt.t;
  tx_close: unit -> unit Lwt.t;
  tx_alloc: int -> 'a Lwt.t;
  tx_release: 'a -> unit Lwt.t;
}

type 'a rx = {
  rx_stream: 'a Lwt_stream.t;
  rx_release: 'a -> unit Lwt.t;
}

type ('a, 'b) t = {
  rx: 'a rx;
  tx: 'b tx;
}

(* A flow of shared memory extents *)
let make ~rx_stream ~rx_release ~tx_send ~tx_release ~tx_close ~tx_alloc =
  let tx = { tx_send; tx_release; tx_close; tx_alloc } in
  let rx = { rx_stream; rx_release } in
  let t, u = Lwt.task () in
  {tx; rx; }

module TX = struct

  let alloc ch len =
    ch.tx.tx_alloc len

  let send ch buf =
    ch.tx.tx_send buf

  let release ch buf =
    ch.tx.tx_release buf

end

module RX = struct

  let recv fn ch =
    Lwt_stream.iter_s fn ch.rx.rx_stream

  let release ch buf =
    ch.rx.rx_release buf

end

let close ch =
  ch.tx.tx_close ()
