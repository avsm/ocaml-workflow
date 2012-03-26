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

(** An Lwt test function *)
type test_fun = string -> unit Lwt.t

(* An Lwt test function that also accepts a file descriptor *)
type test_fun_fd = Lwt_unix.file_descr -> test_fun

(* A client/server function pair (client first, server second *)
type cs = test_fun * test_fun
type cs_fd = test_fun_fd * test_fun_fd

(* Run a client/server test, by forking two independent processes
 * and waiting for them to terminate. Each process will start a 
 * fresh Lwt_main within its sub-process and invoke the relevant
 * test function from the [cs] pair, and terminate immediately
 * after the Lwt_main finishes.
 *)
val run_client_server : string -> cs -> unit -> unit

(* Brackets a [cs] invocation with a common domain socket they can
 * communicate over, and returns the [cs] pair which can be executed
 * by the test runner. The client will wait for a short amount of time
 * to before connecting to the server, which will pass a listen fd to
 * its server test function.
 *)
val with_domain_socket : ?ty:Lwt_unix.socket_type -> string -> cs_fd -> cs
