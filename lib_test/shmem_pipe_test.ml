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

let dprintf fmt =
  let xfn ch = fprintf ch fmt in
  kfprintf xfn stderr "[%d] " (Unix.getpid())

let listen_t () =
  let name = "foo" in
  let fn h =
    let rx, tx_send, tx_release, tx_close, tx_alloc = Shmem_pipe.streams_of_handle h in
    for_lwt i = 0 to 1000 do
      let data = sprintf "data iteration %d\n%!" i in
      lwt ext = tx_alloc (String.length data) in
      let buf = Simplex.buffer (h.Shmem_pipe.tx) ext in
      Lwt_bytes.blit_string_bytes data 0 buf 0 (String.length data);
      tx_send ext;
      return ()
    done >>
    Lwt_unix.sleep 2.0
  in
  let listen_t = Shmem_pipe.listen ~name fn in
  listen_t

let connect_t () =
  let name = "foo" in
  lwt ch = Shmem_pipe.connect ~name () in
  let rx, tx_send, tx_release, tx_close, tx_alloc = Shmem_pipe.streams_of_handle ch in
  let t = Lwt_stream.iter_s
    (fun ext ->
      let buf = Simplex.buffer (ch.Shmem_pipe.rx) ext in
      eprintf "recv: %S\n%!" (Lwt_bytes.to_string buf);
      return ()
    ) rx 
  in
  t

let _ = 
  (* Fork two processes *)
  match Lwt_unix.fork () with
  |0 -> begin (* child *)
    (* sleep for a bit, and then try to connect to the parent *)
    Unix.sleep 1;
    (try
      Lwt_unix.run (connect_t ())
    with Unix.Unix_error (e,_,_) as exn ->
      dprintf "%s (%s)\n%!" (Printexc.to_string exn) (Unix.error_message e));
    dprintf "child: exit\n%!"
  end
  |pid -> begin (* parent *)
    (try
      Lwt_unix.run (listen_t ())
    with Unix.Unix_error (e,_,_) as exn ->
      dprintf "%s (%s)\n%!" (Printexc.to_string exn) (Unix.error_message e));
    dprintf "parent: exit\n%!"
  end
