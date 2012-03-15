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

let listen_t () =
  let name = "foo" in
  let fn handle =
    eprintf "new handle\n%!";
    Lwt_unix.sleep 2.0
  in
  let listen_t = Shmem_pipe.listen ~name fn in
  listen_t

let connect_t () =
  let name = "foo" in
  Shmem_pipe.connect ~name ()

let _ = 
  (* Fork two processes *)
  match Unix.fork () with
  |0 -> begin (* child *)
    (* sleep for a bit, and then try to connect to the parent *)
    Unix.sleep 1;
    (try
      Lwt_unix.run (connect_t ())
    with Unix.Unix_error (e,_,_) as exn ->
      eprintf "%s (%s)\n%!" (Printexc.to_string exn) (Unix.error_message e));
    eprintf "child: exit\n%!"
  end
  |pid -> begin (* parent *)
    (try
      Lwt_unix.run (listen_t ())
    with Unix.Unix_error (e,_,_) as exn ->
      eprintf "%s (%s)\n%!" (Printexc.to_string exn) (Unix.error_message e));
    eprintf "parent: exit\n%!"
  end
