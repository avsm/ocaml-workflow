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

(* Server function which is given a bound (but not listening) fd and its sockaddr,
 * and also an integer the total number of iterations the client will perform. *)
type server_fun = Lwt_unix.file_descr -> Lwt_unix.sockaddr -> int -> unit Lwt.t

(* The client is given a connected file descriptor, and the total number of iterations
 * it is expected to perform on that fd. *)
type client_fun = Lwt_unix.file_descr -> Lwt_unix.sockaddr -> int -> unit Lwt.t

(* Create a server socket, given a string socket path and a total number of expected iterations, 
 * and a server function to process incoming connections. The result is staged with a () to
 * curry it up as a test function *)
val with_server : ?ty:Lwt_unix.socket_type -> Lwt_unix.sockaddr -> int -> server_fun -> unit -> unit Lwt.t

val with_client : ?ty:Lwt_unix.socket_type -> Lwt_unix.sockaddr -> int -> client_fun -> (unit -> unit Lwt.t) Lwt.t

(* Given a list of Lwt threads, run them in independent processes and join until all processes
 * terminal *)
val run_p : ('a -> 'b Lwt.t) list -> 'a -> unit

type procset = { server : server_fun; clients : client_fun list; }

val test_procset_p : name:string -> iters:int -> ?ty:Lwt_unix.socket_type -> procset -> Lwt_unix.sockaddr -> unit -> unit

(* Construct a unique UNIX domain sockaddr *)
val make_unix_sockaddr: ?name:string -> unit -> Lwt_unix.sockaddr

(* Construct a unique TCP sockaddr bound to localhost *)
val make_tcp_sockaddr: ?port:int -> unit -> Lwt_unix.sockaddr
