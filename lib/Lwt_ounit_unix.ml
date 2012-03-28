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

type perf_ctr = {
  execution_time: float;
  pre_gc: Gc.stat;
  post_gc: Gc.stat;
}

let with_perf_ctrs fn a =
  let t1 = Unix.gettimeofday () in
  Gc.compact ();
  let pre_gc = Gc.stat () in
  lwt x = fn a in
  let post_gc = Gc.stat () in
  let t2 = Unix.gettimeofday () in
  let execution_time = t2 -. t1 in
  let perfctr = { execution_time; pre_gc; post_gc } in
  return (x,perfctr)

module Bracket = struct

  (* Lwt bracket that calls a setup function, invokes the test with the result
   * of the setup function, and always calls a teardown irrespective of the
   * test succeeding or failing.  The test function should be idempotent and
   * run multiple times with the result of the set_up function *)
  type ('a,'b) t = {
    set_up: 'a -> 'b Lwt.t;
    test_fun: 'b -> unit Lwt.t;
    tear_down: 'b -> unit Lwt.t;
  }

  (* Apply a bracket to an input value to execute it, and obtain the result
   * of the test function, or an Lwt exception if it failed.
   *)
  let apply br v =
    lwt a = br.set_up v in
    try_lwt
      br.test_fun a >>
      br.tear_down a
    with exn ->
      br.tear_down a >>
      fail exn

  (* Apply a bracket to an input value multiple times, with the setup
   * and teardown functions run just once. The results are returned as
   * a list, with performance counters recorded within it *)
  let apply_n br v iters =
    lwt a = br.set_up v in
    let rec apply_n acc i v =
      match i with
      |0 -> return acc
      |i ->
         lwt ((),r) = with_perf_ctrs br.test_fun v in
         apply_n (r::acc) (i-1) v
    in
    try_lwt
      lwt perf = apply_n [] iters a in
      br.tear_down a >>
      return perf
    with exn ->
      br.tear_down a >>
      fail exn
   
  (* Wrap a test function as a bracket, with an option setup and teardown
   * function to call around the test case.
   *)
  let return ~set_up ?(tear_down=(fun _ -> Lwt.return ())) ?(test_fun=(fun _ -> Lwt.return ())) () =
    { set_up; test_fun; tear_down }
 
  (* Bind a bracket [br2] to take the result of [br1] and apply it.
   * The [br1] setup function will run first, execute the [br1] testcase,
   * and pass the testcase result to the [br2] setup function.
   *)
  let bind br1 br2 =
    let set_up v =
      lwt v1 = br1.set_up v in
      lwt v2 = br2.set_up v1 in
      Lwt.return (v1, v2)
    in
    let tear_down (v1,v2) =
      br2.tear_down v2 >>
      br1.tear_down v1
    in
    let test_fun (v1,v2) =
      br1.test_fun v1 >>
      br2.test_fun v2
    in
    return ~set_up ~tear_down ~test_fun ()

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

(* Convenience wrapper for the domain socket bracket below *)
type sockinfo = {
  sockaddr: Lwt_unix.sockaddr;
  fd: Lwt_unix.file_descr;
}

let with_server_socket ?(ty=Lwt_unix.SOCK_STREAM) br =
  let set_up sockpath = 
    let sockaddr = Lwt_unix.ADDR_UNIX sockpath in
    let fd = Lwt_unix.socket Lwt_unix.PF_UNIX ty 0 in
    Lwt_unix.bind fd sockaddr;
    return {sockaddr; fd}
  in
  let tear_down {sockaddr; fd} =
    Unix.close (Lwt_unix.unix_file_descr fd);
    match sockaddr with
    |Lwt_unix.ADDR_UNIX sockpath ->
      (try Unix.unlink sockpath with _ -> ());
      return ()
    |_ -> return ()
  in
  Bracket.(bind (return ~set_up ~tear_down ()) br)

let with_client_socket ?(ty=Lwt_unix.SOCK_STREAM) br =
  let set_up sockpath = 
    let sockaddr = Lwt_unix.ADDR_UNIX sockpath in
    let fd = Lwt_unix.socket Lwt_unix.PF_UNIX ty 0 in
    let rec connect =
      function
      |100 -> assert_failure "connect"
      |n -> begin
        Lwt_unix.sleep 0.1 >>
        try_lwt 
          lwt () = Lwt_unix.connect fd sockaddr in
          return ()
        with Unix.Unix_error((Unix.ECONNREFUSED|Unix.ENOENT),_,_) ->
          connect (n+1)
      end
    in
    lwt () = connect 0 in
    return {sockaddr; fd}
  in
  let tear_down { sockaddr; fd } =
    Unix.close (Lwt_unix.unix_file_descr fd);
    return ()
  in
  Bracket.(bind (return ~set_up ~tear_down ()) br)

(* A set of brackets with one server and multiple clients, that are all run
 * in independent processes.
 *)
type ('a,'b) procset = {
  server: ('a,'b) Bracket.t;
  clients: ('a,'b) Bracket.t list;
}

(* Test a procset, after framing it with a domain socket thread *)
let test_procset_p ?(ty=Lwt_unix.SOCK_STREAM) procset sockpath () =
  let server = with_server_socket ~ty procset.server in
  let clients = List.map (with_client_socket ~ty) procset.clients in
  Bracket.apply_p (server :: clients) sockpath ()
