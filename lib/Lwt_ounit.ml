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
 * has a unique TestLabel *)
let test_iter label test_fun =
  let rec fn acc =
    function
    |0 -> acc
    |n -> 
      let test = TestLabel (sprintf "%s:%d" label n, TestCase test_fun) in
      fn (test::acc) (n-1)
  in fn [] 

let (>::=) = test_iter
    
(* Main loop to invoke a suite of tests *)
let main ~suite_name ~tests =
  let suite = suite_name >::: tests in
  let verbose = ref false in
  let set_verbose _ = verbose := true in
  Arg.parse
    [("-verbose", Arg.Unit set_verbose, "Run the test in verbose mode.");]
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    ("Usage: " ^ Sys.argv.(0) ^ " [-verbose]");
  try if not (was_successful (run_test_tt ~verbose:!verbose suite)) then exit 1
  with Unix.Unix_error (e,_,_) as exn ->
    eprintf "Unix_error %s: %s\n%!" (Unix.error_message e) (Printexc.to_string exn);
    raise exn
