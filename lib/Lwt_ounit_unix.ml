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

(* Helper utilities to make oUnit/Lwt testing easier *)
open OUnit
open Printf

(* Run an Lwt main loop and terminate right after *)
let lwt_run fn a =
  try
    Lwt_main.run (fn a);
    exit 0
  with
    |Unix.Unix_error (e,_,_) as exn ->
      eprintf "%s (%s)\n%!" (Printexc.to_string exn) (Unix.error_message e);
      exit (-1)
    |exn ->
      eprintf "%d: %s\n%!" (Unix.getpid()) (Printexc.to_string exn);
      exit (-1)
  
(* Wait for a child process to terminate, and fail the test if
 * it is an abnormal exit code *)
let rec test_waitpid ~name pid =
  try
    match Unix.waitpid [] pid with
    |_, Unix.WEXITED st when st = 0 -> ()
    |_,_ -> assert_failure (name^": child process unclean exit")
  with Unix.Unix_error(Unix.EINTR,_,_) ->
    test_waitpid ~name pid

type test_fun = string -> unit Lwt.t
type test_fun_fd = Lwt_unix.file_descr -> test_fun
type cs = test_fun * test_fun
type cs_fd = test_fun_fd * test_fun_fd

(* Fork two processes, which will call the clientfn and serverfn
 * as Lwt threads, with a short delay for the client (to give the 
 * server time to set itself up.
 * The test will wait for both processes to terminate and succeed
 * or fail based on their error codes both being normal.
 *)
let run_client_server name (clientfn, serverfn) () =
  match Unix.fork () with
  |0 -> (* child, server *)
    lwt_run serverfn name
  |pid_server -> begin (* parent *)
    Unix.sleep 1;
    match Unix.fork () with
    |0 -> (* child2, client *)
      lwt_run clientfn name
    |pid_client -> begin (* parent, test runner *)
      (* Wait for both children to terminate *)
      test_waitpid ~name pid_client;
      test_waitpid ~name pid_server;
    end
  end

(* Bracket a client/server function pair with a domain socket
 * that they can communicate between. The server listens, and so
 * the test case should start with an accept, and the client
 * just does a single connect.
 *)
let with_domain_socket ?(ty=Lwt_unix.SOCK_STREAM) name (cfn, sfn) = 
  (* Generate a random sockpath, and do not use tempfile, as 
   * that may be a no-exec mount point *) 
  let open Lwt_unix in
  let sockpath = sprintf "test_%s.%d.sock" name (Random.int 60000) in
  let sockaddr = ADDR_UNIX sockpath in
  (try Unix.unlink sockpath with _ -> ());
  let listen_t name =
    let fd = socket PF_UNIX SOCK_STREAM 0 in
    bind fd sockaddr;
    listen fd 5;
    try_lwt sfn fd name
  in
  let connect_t name =
    let fd = socket PF_UNIX SOCK_STREAM 0 in
    lwt () = connect fd sockaddr in
    try_lwt cfn fd name
  (*    finally close fd *)
  (* XXX The above blocks on MacOS X. This is possibly due to the
   * async job invocation that Lwt_unix.close uses. Why not just call
   * close(2) directly?  NFS biowait blocking perhaps. Needs investigation *)
  in
  connect_t, listen_t

(* TODO remove temporary sockets by making this a bracket *)
