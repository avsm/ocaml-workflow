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

(* The Unix transport channel uses sendmsg/recvmsg, with file descriptors
 * as the RPC metadata *)
module Unix_transport = struct

  type xid = int
  type metadata = Unix.file_descr list
  type endp = Lwt_unix.file_descr

  (* 'a is incoming value, 'b is outgoing value *)
  type ('a,'b) chan = {
    fd: Lwt_unix.file_descr;
    read_buf: string;
    read_iov: Lwt_unix.io_vector list;
    write_buf: string;
  }

  let make_xid =
    let ctr = ref 0 in
    fun () ->
      incr ctr;
      !ctr

  let make fd =
    let length = 8192 in
    let read_buf = String.create length in
    let read_iov = [Lwt_unix.io_vector ~buffer:read_buf ~offset:0 ~length] in
    let write_buf = String.create length in
    { fd; read_buf; read_iov; write_buf }

  let read t =
    lwt len, fds = Lwt_unix.recv_msg ~socket:t.fd ~io_vectors:t.read_iov in
    match len with
    |0 |(-1) ->
      return None
    |_ ->
      let msg = Marshal.from_string t.read_buf 0 in
      return (Some (msg, fds))

  let write t msg metadata =
    let length = Marshal.to_buffer t.write_buf 0 (String.length t.write_buf) msg [] in
    let io_vectors = [Lwt_unix.io_vector ~buffer:t.write_buf ~offset:0 ~length] in
    lwt len' = Lwt_unix.send_msg ~socket:t.fd ~io_vectors ~fds:metadata in
    return ()
end

module RPC = Lwt_rpc.RPC(Unix_transport)
