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

(* Convert a TCP connection into an Lwt_flow *)
open Lwt
open Printf

type handle = {
  (* human-readable name for this handle *)
  name: string;
  (* TCP buffered channel *)
  fd: Lwt_unix.file_descr;
  ic: Lwt_io.input Lwt_io.channel;
  oc: Lwt_io.output Lwt_io.channel;
}

let dprintf fmt =
  let xfn ch = fprintf ch fmt in
  kfprintf xfn stderr "[%d] " (Unix.getpid())

let make_flow h =
  let rx_stream = Lwt_stream.from
    (fun () ->
      match_lwt Lwt_io.read h.ic with
      |"" -> return None
      |x -> return (Some x)
    )
  in
  (* Allocate from the heap *)
  let tx_alloc len = return (String.create len) in
  (* Release is a NOOP as the garbage collector will do it *)
  let rx_release buf = return () in
  let tx_release buf = return () in
  (* Close is a normal TCP close *)
  let tx_close () =
    (* XXX Do not use buggy Lwt_unix close here yet and hence
     * not Lwt_io.close *)
    lwt () = Lwt_io.flush h.oc in
    (try Unix.close (Lwt_unix.unix_file_descr h.fd) with _ -> ());
    return ()
  in
  let tx_send buf =
    try_lwt Lwt_io.write h.oc buf >> return true
    with exn -> return false
  in
  Lwt_flow.make ~rx_stream ~rx_release ~tx_send ~tx_release ~tx_close ~tx_alloc

let string_of_sockaddr =
  let open Unix in
  function
  |Unix.ADDR_UNIX x -> sprintf "unix:%s\n%!" x
  |Unix.ADDR_INET (s,p) -> sprintf "tcp:%s:%d" (string_of_inet_addr s) p

let listen fd =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  Lwt_unix.listen fd 10;
  Lwt_stream.from (fun () ->
    try_lwt
      lwt afd, sa = Lwt_unix.accept fd in
      let name = string_of_sockaddr sa in
      let ic = Lwt_io.(of_fd ~mode:input afd) in
      let oc = Lwt_io.(of_fd ~mode:output afd) in
      let h = { ic; oc; name; fd=afd } in
      return (Some h)
    with exn ->
       return None
  )

(* Assume a connected fd for now, as the test framework does this *)
let connect fd =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let name = "tcp:connect" in
  let ic = Lwt_io.(of_fd ~mode:input fd) in
  let oc = Lwt_io.(of_fd ~mode:output fd) in
  let h = { ic; oc; name; fd } in
  return h
