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

let perf_ctr_to_string =
  function
  |None -> "?"
  |Some p -> sprintf "%f\n" p.execution_time

let with_perf_ctrs1 fn a =
  Gc.compact ();
  let t1 = Unix.gettimeofday () in
  let pre_gc = Gc.stat () in
  lwt x = fn a in
  let t2 = Unix.gettimeofday () in
  Gc.compact ();
  let post_gc = Gc.stat () in
  let execution_time = t2 -. t1 in
  let perfctr = { execution_time; pre_gc; post_gc } in
  return (x,perfctr)

let with_perf_ctrs2 fn a b =
  Gc.compact ();
  let t1 = Unix.gettimeofday () in
  let pre_gc = Gc.stat () in
  lwt x = fn a b in
  let t2 = Unix.gettimeofday () in
  Gc.compact ();
  let post_gc = Gc.stat () in
  let execution_time = t2 -. t1 in
  let perfctr = { execution_time; pre_gc; post_gc } in
  return (x,perfctr)

let with_perf_n ~name ~iters fn =
  let p = Array.create iters None in
  fun a b ->
    try_lwt
      for_lwt i = 0 to iters - 1 do
        Gc.compact ();
        let t1 = Unix.gettimeofday () in
        let pre_gc = Gc.stat () in
        lwt x = fn a b in
        let t2 = Unix.gettimeofday () in
        Gc.compact ();
        let post_gc = Gc.stat () in
        let execution_time = t2 -. t1 in
        let perfctr = { execution_time; pre_gc; post_gc } in
        p.(i) <- Some perfctr;
        return ()
      done 
    finally
      Array.iteri (fun i p ->
        eprintf "%s %d %s\n%!" name i (perf_ctr_to_string p)
      ) p;
      return ()

type server_fun = Lwt_unix.file_descr -> Lwt_unix.sockaddr -> int -> unit Lwt.t
type client_fun = Lwt_unix.file_descr -> Lwt_unix.sockaddr -> (int -> unit Lwt.t)

let with_server ?(ty=Lwt_unix.SOCK_STREAM) sockpath iters (fn:server_fun) () =
  let sockaddr = Lwt_unix.ADDR_UNIX sockpath in
  let fd = Lwt_unix.socket Lwt_unix.PF_UNIX ty 0 in
  Lwt_unix.bind fd sockaddr;
  try_lwt
    fn fd sockaddr iters
  finally
    begin
    Unix.close (Lwt_unix.unix_file_descr fd);
    (match sockaddr with
    |Lwt_unix.ADDR_UNIX sockpath -> (try Unix.unlink sockpath with _ -> ());
    |_ -> ());
    return ();
    end

let with_client ?(ty=Lwt_unix.SOCK_STREAM) sockpath iters iterfn =
  let sockaddr = Lwt_unix.ADDR_UNIX sockpath in
  let fd = Lwt_unix.socket Lwt_unix.PF_UNIX ty 0 in
  let rec connect =
    function
    |100 -> assert_failure "connect"
    |n -> begin
      Lwt_unix.sleep 0.1 >>
      try_lwt 
        Lwt_unix.connect fd sockaddr
      with Unix.Unix_error((Unix.ECONNREFUSED|Unix.ENOENT),_,_) ->
        connect (n+1)
    end
  in
  lwt () = connect 0 in
  let iter_t () = 
    try_lwt
      iterfn fd sockaddr iters
    finally 
      Unix.close (Lwt_unix.unix_file_descr fd);
      return ()
  in
  return iter_t

let run_p fns v =
  (* TODO: add a logger here that could capture the process output *)
  let rec fork_all acc =
    function
    |fn::tl -> begin
      match Lwt_unix.fork () with
      |0 -> begin (* child *)
        try lwt_run fn v
        with exn -> exit 1
      end
    |pid -> fork_all (pid::acc) tl
    end
    |[] -> List.rev acc
  in      
  let pids = fork_all [] fns in
  test_waitpid true pids

type procset = {
 server: server_fun;
 clients: client_fun list;
}

(* Test a procset, after framing it with a domain socket thread *)
let test_procset_p ~name ~iters ?(ty=Lwt_unix.SOCK_STREAM) ps sockpath () =
  let server = with_server ~ty sockpath iters ps.server in
  (* Connect all the clients and wait until they are ready to go *)
  let clients = List.map
    (fun cl ->
       fun () ->
         lwt cfn = with_client ~ty sockpath iters cl in
         cfn ()
    ) ps.clients
  in 
  run_p (server::clients) ()

