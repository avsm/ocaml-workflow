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

type server_fun = Lwt_unix.file_descr -> Lwt_unix.sockaddr -> int -> unit Lwt.t
type client_fun = Lwt_unix.file_descr -> Lwt_unix.sockaddr -> int -> unit Lwt.t

val with_server :
  ?ty:Lwt_unix.socket_type -> string -> int -> server_fun -> unit -> unit Lwt.t
val with_client :
  ?ty:Lwt_unix.socket_type -> string -> int -> client_fun -> (unit -> unit Lwt.t) Lwt.t
val run_p : ('a -> 'b Lwt.t) list -> 'a -> unit
type procset = { server : server_fun; clients : client_fun list; }
val test_procset_p :
  name:string ->
  iters:int -> ?ty:Lwt_unix.socket_type -> procset -> string -> unit -> unit

