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

(* Request/responses in Lwt, with custom serialisers *)
open Lwt
open Printf

(* Our transport channels are duplex, reliable, and can transport both
 * data and metadata.
 *)
module type TRANSPORT = sig
  type xid
  type metadata
  type endp
  (* 'a is written message, 'b is read message *)
  type ('a,'b) chan

  val make_xid : unit -> xid
  val make : endp -> ('a,'b) chan
  val write : ('a,'b) chan -> 'a -> metadata -> unit Lwt.t
  val read : ('a,'b) chan -> ('b * metadata) option Lwt.t
end

(* The underlying transport may be uni-directional, or 
 * bi-directional (and hence multiplex requests/responses 
 * on the marshalled message to disambiguate them *)
module RPC(T:TRANSPORT) = struct
  (* We just marshal a message directly as an xid *)
  type 'a request = T.xid * 'a
  type 'b response = T.xid * 'b

  module Client = struct
    type ('a,'b) t = {
      transport: ('a request, 'b response) T.chan;
      reader: unit Lwt.t;
      waiters: (T.xid, ('b * T.metadata) option Lwt.u) Hashtbl.t;
    }

    (* Connector will send requests and receive responses *)
    let client t =
      let waiters = Hashtbl.create 7 in
      (* Reader thread to dispatch answers *)
      let reader =
        let live = ref true in
        while_lwt !live do
          match_lwt T.read t with
          |None -> live := false; return ()
          |Some (msg, meta) -> begin
            let (xid,resp) = msg in
            try 
              let u = Hashtbl.find waiters xid in 
              Hashtbl.remove waiters xid;
              Lwt.wakeup u (Some (resp,meta));
              return ()
            with Not_found ->
              return () (* Message timed out on the client side, perhaps *)
          end  
        done
      in
      { transport=t; waiters; reader }

    let send (t:('a,'b) t) (req:'a) metadata =
      let xid = T.make_xid () in
      let th,u = Lwt.task () in (* XXX does this need to be cancelable? *)
      Hashtbl.add t.waiters xid u;
      Lwt.on_cancel th (fun () ->
        try Hashtbl.remove t.waiters xid
        with Not_found -> ()
      );
      let (msg:'a request) = xid, req in
      T.write t.transport msg metadata >>
      th

    let close t =
      Hashtbl.iter (fun xid u -> Lwt.wakeup u None) t.waiters;
      Lwt.cancel t.reader;
      return ()
  end

  module Server = struct
    type ('a, 'b) t = {
      transport: ('b response,'a request) T.chan;
      listen_t: unit Lwt.t;
    }

    let server t mapfn =
      let active_reqs = Lwt_sequence.create () in
      let listen_t = 
        let live = ref true in
        while_lwt !live do
          match_lwt T.read t with
          |None -> live := false; return ()
          |Some ((xid,metadata),(req:'a)) ->
            let resp_t =
              lwt (res,metadata) = mapfn req metadata in
              T.write t (xid,res) metadata
            in
            (* TODO store blocked resp_t in active_reqs so that they
             * can be cancelled if the listen thread is stopped *)
            return ()   
        done
      in
      { transport=t; listen_t }
  end
end
