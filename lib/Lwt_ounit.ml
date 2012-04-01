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

open OUnit
open Printf

(* Returns true if the result list contains successes only.
   Copied from oUnit source as it isnt exposed by the mli *)
let rec was_successful =
  function
    | [] -> true
    | RSuccess _::t
    | RSkip _::t ->
        was_successful t
    | RFailure _::_
    | RError _::_
    | RTodo _::_ ->
        false

(* Convert a test into multiple iterations, each of which
 * has a unique TestLabel. *)
let test_iter label test_fun =
  let rec fn acc =
    function
    |0 -> acc
    |n -> 
      let test = TestLabel (sprintf "%s:%d" label n, TestCase test_fun) in
      fn (test::acc) (n-1)
  in fn [] 

let (>::=) = test_iter

let repeat v i =
  let rec fn acc =
    function
    |0 -> acc
    |n -> fn (v :: acc) (n-1)
  in fn [] i

(* Main loop to invoke a suite of tests *)
let main ~suite_name ~tests =
  let suite = suite_name >::: tests in
  let verbose = ref true in
  let set_verbose _ = verbose := true in
  Arg.parse
    [("-verbose", Arg.Unit set_verbose, "Run the test in verbose mode.");]
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    ("Usage: " ^ Sys.argv.(0) ^ " [-verbose]");
  try if not (was_successful (run_test_tt ~verbose:!verbose suite)) then exit 1
  with Unix.Unix_error (e,_,_) as exn ->
    eprintf "Unix_error %s: %s\n%!" (Unix.error_message e) (Printexc.to_string exn);
    raise exn

type perf_test = num_clients:int -> client_iters:int -> data_size:int -> test_reps:int -> test list
(* Main loop parameterised by data size, client iterations suitable for performance
 * comparisons by external scripts *)
let main_perf ~suite_name ~(tests:perf_test) =
  let verbose = ref true in
  let set_verbose _ = verbose := true in
  let client_iters = ref 1 in
  let data_size = ref 1 in
  let num_clients = ref 1 in
  let test_reps = ref 1 in
  Arg.parse
    [("-verbose", Arg.Unit set_verbose, "Run the test in verbose mode.");
     ("-client-iters", Arg.Set_int client_iters, "Client RPC iterations.");
     ("-data-size", Arg.Set_int data_size, "Size of data packets to send.");
     ("-test-reps", Arg.Set_int test_reps, "Repeat each test N times.");
     ("-num-clients", Arg.Set_int num_clients, "Number of parallel clients.");
    ]
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    ("Usage: " ^ Sys.argv.(0) ^ " [-verbose]");
  (* Specialise the test list with the parameters *)
  let tests = tests ~num_clients:!num_clients ~client_iters:!client_iters
    ~data_size:!data_size ~test_reps:!test_reps in
  let suite = suite_name >::: tests in
  try if not (was_successful (run_test_tt ~verbose:!verbose suite)) then exit 1
  with Unix.Unix_error (e,_,_) as exn ->
    eprintf "Unix_error %s: %s\n%!" (Unix.error_message e) (Printexc.to_string exn);
    raise exn
