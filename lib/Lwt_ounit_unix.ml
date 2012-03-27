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
open Lwt

(* Run an Lwt main loop and return the value from the thread *)
let lwt_run fn a =
  try
    let _ = Lwt_main.run (fn a) in
    exit 0
  with
    |Unix.Unix_error (e,_,_) as exn ->
      eprintf "%s (%s)\n%!" (Printexc.to_string exn) (Unix.error_message e);
      exit (-1)
    |exn ->
      eprintf "%d: %s\n%!" (Unix.getpid()) (Printexc.to_string exn);
      exit (-1)
  
(* Wait for a set of child processes to terminate, and fail the test if
 * any had an abnormal exit code *)
let rec test_waitpid acc =
  function
  |[] -> "child processes" @? acc
  |pid::tl as pids -> begin
    try
      match Unix.waitpid [] pid with
      |_, Unix.WEXITED st when st = 0 ->
        test_waitpid acc tl
      |_,_ ->
        test_waitpid false tl
    with Unix.Unix_error(Unix.EINTR,_,_) ->
      test_waitpid acc pids
   end

(* Lwt version of the test function that blocks *)
type test_fun = unit -> unit Lwt.t

(* Lwt bracket that can do test setup, execute the function, and cleanup *)
type ('a,'b,'c) bracket_fun = {
  set_up: 'a -> 'b Lwt.t;
  test_fun: 'b -> 'c Lwt.t;
  tear_down: 'b -> unit Lwt.t;
}

(* Lwt version of the oUnit bracket, with an initial argument to pass to
 * the setup function (unlike oUnit which just takes a unit to set_up *)
let bracket_s br v =
  lwt a = br.set_up v in
  try_lwt
    lwt x = br.test_fun a in
    br.tear_down a >> 
    return x
  with exn ->
    lwt r = br.tear_down a in
    fail exn

let bracket1 set_up test_fun tear_down =
  { set_up; test_fun; tear_down }
  
(* Map an Lwt bracket [br] with a set_up/tear_down function that happens
 * before and after in [br] set_up/tear_down functions. *)
let bracket_map set_up testfn br tear_down =
  bracket1 set_up (fun v -> testfn v >>= bracket_s br) tear_down

let bracket_bind br1 br2 =
  bracket1 br1.set_up (fun v -> br1.test_fun v >>= bracket_s br2) br1.tear_down

(* Process set that operate in parallel in independent processes, with their
 * own Lwt_main loops *)
type ('a,'b,'c) procset = ('a,'b,'c) bracket_fun list

(* Fork a procset as a parallel set of processes, each with their own
 * bracket functions. Wrap the bracket functions to exit from the child
 * processes.  The test will wait for both processes to terminate and
 * set error codes based on return values.
 *)
let bracket_p procset v () =
  (* TODO: add a logger here that could capture the process output *)
  let rec fork_all acc =
    function
    |br::tl -> begin
      match Lwt_unix.fork () with
      |0 -> begin (* child *)
        try lwt_run (bracket_s br) v
        with exn -> exit 1
      end
      |pid -> fork_all (pid::acc) tl
    end
    |[] -> List.rev acc
  in      
  let pids = fork_all [] procset in
  test_waitpid true pids

(* Procset with a special first one that acts as a server, and the
 * rest are clients *)
type ('a,'b,'c) procset_server = {
  server: ('a,'b,'c) bracket_fun;
  clients: ('a,'b,'c) procset;
}

(* Turn a server procset into a normal procset that can be run by bracket_p *)
let procset_of_server ps = ps.server :: ps.clients

(* Bracket a procset_server with a domain socket pair
 * that they can communicate to the clients with. The server listens and
 * the server testfun should start with an accept, and the clients all
 * can connect immediately via their fd.
 *)
type sockinfo = {
  sockaddr: Lwt_unix.sockaddr;
  fd: Lwt_unix.file_descr;
}

let bracket_domain_socket ~ty ps_cs =
  (* Generate a random sockpath, and do not use tempfile, as 
   * that may be a no-exec mount point *) 
  let open Lwt_unix in
  let set_up sockpath = 
    let sockaddr = ADDR_UNIX sockpath in
    let fd = socket PF_UNIX ty 0 in
    return {sockaddr; fd}
  in
  let listen_t {sockaddr; fd} =
    bind fd sockaddr;
    return fd
  in
  let connect_t {sockaddr; fd} =
    let rec retry =
      function
      |100 -> assert_failure "connect"
      |n -> begin
        Lwt_unix.sleep 0.1 >>
        try_lwt 
          lwt () = connect fd sockaddr in
          return fd
        with Unix.Unix_error(Unix.ECONNREFUSED,_,_) ->
          retry (n+1)
      end
    in retry 0
  in
  let tear_down {sockaddr; fd} =
    match sockaddr with
    |ADDR_UNIX sockpath ->
      (try Unix.unlink sockpath with _ -> ());
      return ()
    |_ -> return ()
  (*    close fd *)
  (* XXX The above blocks on MacOS X. This is possibly due to the
   * async job invocation that Lwt_unix.close uses. Why not just call
   * close(2) directly?  NFS biowait blocking perhaps. Needs investigation *)
  in
  let server = bracket_map set_up listen_t ps_cs.server tear_down in
  let clients = List.map (fun c -> bracket_map set_up connect_t c (fun _ -> return ())) ps_cs.clients in
  { server; clients }

(* TODO remove temporary sockets by making this a bracket *)
