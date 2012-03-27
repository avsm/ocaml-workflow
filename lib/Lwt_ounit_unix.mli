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

module Bracket : sig
  type ('a, 'b, 'c) t

  val return : set_up:('a -> 'b Lwt.t) -> ?tear_down:('b -> unit Lwt.t) ->
      ('b -> 'c Lwt.t) -> ('a, 'b, 'c) t

  val bind : ('a, 'b, 'c) t -> ('c, 'd, 'e) t -> ('a, 'b, 'e) t

  val apply : ('a, 'b, 'c) t -> 'a -> 'c Lwt.t
  val apply_p : ('a, 'b, 'c) t list -> 'a -> unit -> unit
end

type ('a, 'b, 'c) procset_server = {
  server : ('a, 'b, 'c) Bracket.t;
  clients : ('a, 'b, 'c) Bracket.t list;
}

val procset_of_server : ('a, 'b, 'c) procset_server -> ('a, 'b, 'c) Bracket.t list

type sockinfo ={
  sockaddr : Lwt_unix.sockaddr; 
  fd : Lwt_unix.file_descr; 
}

val bracket_domain_socket :
  ?ty:Lwt_unix.socket_type ->
  (Lwt_unix.file_descr, 'a, 'b) procset_server ->
  (string, sockinfo, 'b) procset_server
