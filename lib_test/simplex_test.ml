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

(* Given a set of ring sizes and allocation sizes to do within
 * the ring, run all combinations with the test function *)
let with_alloc_pattern name ~total_sizes ~alloc_sizes test =
  List.flatten (
    List.map (fun total_size ->
      List.map (fun alloc_size ->
        let shm = Shm.open_anonymous () in
        let ring = Simplex.attach_tx shm total_size in
        test ~ring ~total_size ~alloc_size
      ) alloc_sizes
    ) total_sizes
  )

let test_with_alloc_pattern name ~total_sizes ~alloc_sizes test =
  with_alloc_pattern name ~total_sizes ~alloc_sizes
    (fun ~ring ~total_size ~alloc_size ->
       sprintf "%s_%d_%d" name total_size alloc_size >::
       test ~ring ~total_size ~alloc_size
    )   

(* Allocate a single value and release it repeatedly, checking
 * that allocation never fails *)
let allocate_release_test =
  let total_sizes = [4096; 8192; 16384] in
  let alloc_sizes = [1;2;3;7;8;128;256;1024;1025] in
  test_with_alloc_pattern "allocate_release" ~total_sizes ~alloc_sizes
    (fun ~ring ~total_size ~alloc_size () ->
      for i = 0 to 100000 do
        match Simplex.alloc ring alloc_size with
        |Some extent ->
          assert_equal alloc_size (Simplex.length extent);
          Simplex.release extent;
        |None -> assert_failure "allocate failed"
       done
    )

(* Allocate until the ring is full, and then verify that it does
 * indeed have no space left *)
let alloc_until_full =
  let total_sizes = [4096; 8192; 32768] in
  let alloc_sizes = [ 128; 256; 512; 4096 ] in
  test_with_alloc_pattern "allocate_until_full" ~total_sizes ~alloc_sizes
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

(* Allocate several rings and make sure that the native/foreign
 * membership tests works (including between GCs *)
let test_membership =
  let total_sizes = [ 4096; 8192; 32768 ] in
  let alloc_sizes = [ 128; 256; 512; 1024 ] in
  let a_ring = Simplex.attach_tx (Shm.open_anonymous ()) 8192 in
  (* Test that extent membership tests work *)
  let test_extents =
    test_with_alloc_pattern "test_extents" ~total_sizes ~alloc_sizes
      (fun ~ring ~total_size ~alloc_size () ->
         Gc.compact ();
         let extents = List.map (fun _ ->
           match Simplex.alloc ring alloc_size with
           |None -> assert_failure "alloc failed"
           |Some ext -> ext
         ) [1;2;3;4] in
         List.iter (fun extent ->
            assert_bool "is_member" (Simplex.extent_is_member ring extent);
            assert_bool "isnt_member" (not (Simplex.extent_is_member a_ring extent));
         ) extents
      ) in
  (* Test that bigarray membership tests work *)
  let test_bufs =
     test_with_alloc_pattern "test_bigarrays" ~total_sizes ~alloc_sizes
      (fun ~ring ~total_size ~alloc_size () ->
         Gc.compact ();
         let bufs = List.map (fun _ ->
           match Simplex.alloc ring alloc_size with
           |None -> assert_failure "alloc failed"
           |Some extent -> Simplex.buffer extent
         ) [1;2;3;4] in
         List.iter (fun buf ->
           assert_bool "is_member" (Simplex.buf_is_member ring buf);
           assert_bool "isnt_member" (not (Simplex.buf_is_member a_ring buf));
         ) bufs
      ) in
  test_extents @ test_bufs

let _ = 
  let tests = allocate_release_test @ test_membership in
  Lwt_ounit.main ~suite_name:"simplex_test" ~tests
