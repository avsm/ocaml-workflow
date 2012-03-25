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

(* Bounded queue *)
open Lwt

type 'a t = {
  queue: 'a Queue.t;
  waiters: unit Lwt.u Lwt_sequence.t;
  mutable max_length: int option;
}

let create ?(max_length=None) () =
  let queue = Queue.create () in
  let waiters = Lwt_sequence.create () in
  { queue; max_length; waiters }

let push v t =
  match t.max_length with
  |Some len when len >= Queue.length t ->
    let t,u = Lwt.task () in
    let node = Lwt_sequence.add_r u t.waiters;
    Lwt.on_cancel t (fun () -> Lwt_sequence.remove node);
    lwt () = t in
    Queue.push v t;
    return ()
  |Some len | None ->  
    Queue.push v t;
    return ()

let take t v =
  Queue.take t v

let take_opt t v =
  try Some (Quake.take t v)
  with Empty -> None

let length t = Queue.length t.queue
