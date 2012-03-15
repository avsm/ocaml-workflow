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

(* Bindings to shared memory file descriptor functions *)

type shm_descr = Unix.file_descr

module Raw = struct
  external shm_open: name:string -> rw:bool -> creat:bool -> excl:bool ->
    trunc:bool -> Unix.file_descr = "ocaml_shm_open"
  external shm_unlink: name:string -> unit = "ocaml_shm_unlink"
end

let open_shm ?(rw=true) ?(creat=true) ?(excl=false) ?(trunc=false) name =
  Raw.shm_open ~name ~rw ~creat ~excl ~trunc

let unlink name =
  Raw.shm_unlink ~name

let open_anonymous ?(rw=true) ?(creat=true) ?(excl=false) ?(trunc=false) () =
  (* XXX Make up a random name, in a racy way. Needs anonymous maps
   * from FreeBSD to be cured of this malady. *)
  let name = Printf.sprintf "%Lu" (Random.int64 (Int64.max_int)) in
  let fd = open_shm ~rw ~creat ~excl ~trunc name in
  unlink name;
  fd

external unix_descr_of_shm : shm_descr -> Unix.file_descr = "%identity"
external shm_of_unix_descr : Unix.file_descr -> shm_descr = "%identity"
