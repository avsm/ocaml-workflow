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
open Lwt_ounit
open Lwt_ounit_unix

let dprintf fmt =
  let xfn ch = fprintf ch fmt in
  kfprintf xfn stderr "[%d] " (Unix.getpid ())

let server num_clients fd sa client_iters =
  let listen_stream = Shm_pipe.listen fd in
  let th,u = Lwt.task () in
  let n = ref 0 in
  let process h =
    let rx, tx_send, tx_release, tx_close, tx_alloc = Shm_pipe.streams_of_handle h in
    lwt () =
      for_lwt i = 0 to client_iters - 1 do
        let data = sprintf "%d*\n%!" i in
        lwt ext = tx_alloc (String.length data) in
        let buf = Simplex.buffer ext in
        Lwt_bytes.blit_string_bytes data 0 buf 0 (String.length data);
        tx_send ext >>
        return ()
      done
    in
    tx_close () >>
    return ()
  in
  let listen_t = Lwt_stream.iter_s (fun h ->
    lwt () = process h in
    incr n;
    if !n >= num_clients then Lwt.wakeup u ();
    return ()
  ) listen_stream in
  lwt () = th in
  Lwt.cancel listen_t;
  return ()

let client fd sockaddr num_iters =
  lwt ch = Shm_pipe.connect fd in
  let rx, tx_send, tx_release, tx_close, tx_alloc = Shm_pipe.streams_of_handle ch in
  let t = Lwt_stream.iter_s
    (fun ext ->
      let _ = Simplex.buffer ext in
      tx_release ext
    ) rx 
  in
  tx_close () >>
  t

let shm_pipe_test ~rpc_iters =
  let open Lwt_ounit_unix in
  let clients = [ client; ] in
  let ps = { server = server (List.length clients); clients } in
  (* Generate a random sockpath, and do not use tempfile, as 
   * that may be a no-exec mount point *) 
  let sockpath = sprintf "shm_pipe_test.%d.sock" (Random.int 10000) in
  "rpc_ping_smoke" >::= test_procset_p ~name:"rpc_ping"
    ~ty:Lwt_unix.SOCK_DGRAM ~iters:rpc_iters ps sockpath

let _ =
  let tests = shm_pipe_test ~rpc_iters:10000 10 in
  Lwt_ounit.main ~suite_name:"shm_pipe_test" ~tests
