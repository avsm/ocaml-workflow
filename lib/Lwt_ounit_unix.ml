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
 * any of the process set had an abnormal exit code *)
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

module Bracket = struct
  (* Lwt bracket that calls a setup function, invokes the test with the result
   * of the setup function, and always calls a teardown irrespective of the
   * test succeeding or failing.
   *)
  type ('a,'b,'c) t = {
    set_up: 'a -> 'b Lwt.t;
    test_fun: 'b -> 'c Lwt.t;
    tear_down: 'b -> unit Lwt.t;
  }

  (* Apply a bracket to an input value to execute it, and obtain the result
   * of the test function, or an Lwt exception if it failed.
   *)
  let apply br v =
    lwt a = br.set_up v in
    try_lwt
      lwt x = br.test_fun a in
      br.tear_down a >> 
      return x
    with exn ->
      lwt r = br.tear_down a in
      fail exn

  (* Wrap a test function as a bracket, with an option setup and teardown
   * function to call around the test case.
   *)
  let return ~set_up ?(tear_down=(fun _ -> Lwt.return ())) test_fun =
    { set_up; test_fun; tear_down }
 
  (* Bind a bracket [br2] to take the result of [br1] and apply it.
   * The [br1] setup function will run first, execute the [br1] testcase,
   * and pass the testcase result to the [br2] setup function.
   *)
  let bind br1 br2 =
    return ~set_up:br1.set_up ~tear_down:br1.tear_down 
      (fun v -> br1.test_fun v >>= apply br2)

  (* Fork a procset as a parallel set of processes, each with their own
   * bracket functions that in completely independent Lwt loops.
   * The test will wait for both processes to terminate and set error
   * codes based on return values.
   *)
  let apply_p procset v () =
    (* TODO: add a logger here that could capture the process output *)
    let rec fork_all acc =
      function
      |br::tl -> begin
        match Lwt_unix.fork () with
        |0 -> begin (* child *)
          try lwt_run (apply br) v
          with exn -> exit 1
        end
        |pid -> fork_all (pid::acc) tl
      end
      |[] -> List.rev acc
    in      
    let pids = fork_all [] procset in
    test_waitpid true pids

   let (>>=) = bind
end

(* A set of brackets with one server and multiple clients, that are all run
 * in independent processes.
 *)
type ('a,'b,'c) procset_server = {
  server: ('a,'b,'c) Bracket.t;
  clients: ('a,'b,'c) Bracket.t list;
}

(* Turn a server procset into a normal procset that can be run by bracket_p *)
let procset_of_server ps =
  ps.server :: ps.clients

(* Bracket a procset_server with a domain socket pair
 * that they can communicate to the clients with. The server listens and
 * the server testfun should start with an accept, and the clients all
 * can connect immediately via their fd.
 *)
type sockinfo = {
  sockaddr: Lwt_unix.sockaddr;
  fd: Lwt_unix.file_descr;
}

let bracket_domain_socket ?(ty=Lwt_unix.SOCK_STREAM) cs =
  let open Lwt_unix in
  (* Common setup function for both clients and server *)
  let set_up sockpath = 
    let sockaddr = ADDR_UNIX sockpath in
    let fd = socket PF_UNIX ty 0 in
    return {sockaddr; fd}
  in
  (* Server just binds (but not listens, as DGRAM sockets dont *)
  let listen_t {sockaddr; fd} =
    bind fd sockaddr;
    return fd
  in
  (* Clients retry connections as server will be setting up in parallel *)
  let connect_t {sockaddr; fd} =
    let rec retry =
      function
      |100 -> assert_failure "connect"
      |n -> begin
        Lwt_unix.sleep 0.1 >>
        try_lwt 
          lwt () = connect fd sockaddr in
          return fd
        with Unix.Unix_error((Unix.ECONNREFUSED|Unix.ENOENT),_,_) ->
          retry (n+1)
      end
    in retry 0
  in
  (* Tear down is only for the server *)
  let tear_down {sockaddr; fd} =
  (* XXX The below close blocks on MacOS X if done as Lwt_unix.close.
   * This is possibly due to the * async job invocation . Why not just call
   * close(2) directly?  NFS biowait blocking perhaps. Needs investigation *)
    Unix.close (Lwt_unix.unix_file_descr fd);
    match sockaddr with
    |ADDR_UNIX sockpath ->
      (try Unix.unlink sockpath with _ -> ());
      return ()
    |_ -> return ()
  in
  let server = Bracket.(return ~set_up ~tear_down listen_t >>= cs.server) in
  let clients = List.map (fun c -> Bracket.(return ~set_up ~tear_down connect_t >>= c)) cs.clients in
  { server; clients }

