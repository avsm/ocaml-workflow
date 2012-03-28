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

type perf_ctr = {
  execution_time: float;
  pre_gc: Gc.stat;
  post_gc: Gc.stat;
}

val with_perf_ctrs : ('a -> 'b Lwt.t) -> 'a -> ('b * perf_ctr) Lwt.t

module Bracket : sig
  type ('a, 'b) t

  val return : set_up:('a -> 'b Lwt.t) -> ?tear_down:('b -> unit Lwt.t) ->
      ?test_fun:('b -> unit Lwt.t) -> unit -> ('a, 'b) t

  val bind : ('a, 'b) t -> ('b, 'c) t -> ('a, ('b * 'c)) t

  val apply : ('a, 'b) t -> 'a -> unit Lwt.t

  val apply_p : ('a, 'b) t list -> 'a -> unit -> unit

  (* Apply a bracket multiple times and record the performance results
   * for each iteration. *)
  val apply_n : ('a, 'b) t -> 'a -> int -> perf_ctr list Lwt.t
end

type sockinfo = {
  sockaddr : Lwt_unix.sockaddr; 
  fd : Lwt_unix.file_descr; 
}

val with_server_socket : ?ty:Lwt_unix.socket_type -> (sockinfo, 'a) Bracket.t -> (string, sockinfo * 'a) Bracket.t
val with_client_socket : ?ty:Lwt_unix.socket_type -> (sockinfo, 'a) Bracket.t -> (string, sockinfo * 'a) Bracket.t

type ('a, 'b) procset = {
  server : ('a, 'b) Bracket.t;
  clients : ('a, 'b) Bracket.t list;
}

val test_procset_p: ?ty:Lwt_unix.socket_type -> (sockinfo, 'a) procset -> string -> unit -> unit
