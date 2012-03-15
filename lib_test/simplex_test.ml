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

(* Test repeated allocation and release on a single shared ring, with
 * different sized headers *)

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

(* Given a set of ring sizes and allocation sizes to do within
 * the ring, run all combinations with the test function *)
let with_alloc_pattern name ~total_sizes ~alloc_sizes test =
  List.flatten (
    List.map (fun total_size ->
      List.map (fun alloc_size ->
        let shm = Shm.open_anonymous () in
        let ring = Simplex.attach_tx shm total_size in
        sprintf "%s_%d_%d" name total_size alloc_size >::
        test ~ring ~total_size ~alloc_size
      ) alloc_sizes
    ) total_sizes
  )

(* Allocate a single value and release it repeatedly, checking
 * that allocation never fails *)
let allocate_release_test =
  let total_sizes = [4096; 8192; 16384] in
  let alloc_sizes = [1;2;3;7;8;128;256;1024;1025] in
  with_alloc_pattern "allocate_release" ~total_sizes ~alloc_sizes
    (fun ~ring ~total_size ~alloc_size () ->
      for i = 0 to 100000 do
        match Simplex.alloc ring alloc_size with
        |Some extent ->
          assert_equal alloc_size (Simplex.length extent);
          Simplex.release ring extent;
        |None -> assert_failure "allocate failed"
       done
    )

(* Allocate until the ring is full, and then verify that it does
 * indeed have no space left *)
let alloc_until_full =
  let total_sizes = [4096; 8192; 32768] in
  let alloc_sizes = [ 128; 256; 512; 4096 ] in
  with_alloc_pattern "allocate_until_full" ~total_sizes ~alloc_sizes
    (fun ~ring ~total_size ~alloc_size () ->
      let rec fill_tx acc =
        (* allocate until we get a short alloc or a failure *)
        match Simplex.alloc ring alloc_size with
        |Some extent ->
           if (Simplex.length extent) != alloc_size then acc
           else fill_tx (acc+1)
        |None -> acc
      in
      assert_bool "ring has space" (Simplex.has_free_space ring);
      let filled = fill_tx 0 in
      assert_equal filled (total_size / alloc_size);
      assert_bool "ring is full" (not (Simplex.has_free_space ring))
    )

let _ =
  let suite = "Simplex" >::: allocate_release_test in
  let verbose = ref false in
  let set_verbose _ = verbose := true in
  Arg.parse
    [("-verbose", Arg.Unit set_verbose, "Run the test in verbose mode.");]
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    ("Usage: " ^ Sys.argv.(0) ^ " [-verbose]");
  if not (was_successful (run_test_tt ~verbose:!verbose suite)) then
  exit 1
