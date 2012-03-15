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

(* Fast bi-directional shared-memory, with reliable acks and flow control *)
open Lwt
open Printf

type handle = {
  (* metadata pipe *)
  fd: Lwt_unix.file_descr;
  (* human-readable name for this handle *)
  name: string;
  (* Unidirectional data channels *)
  tx: [`tx] Simplex.ring;
  rx: [`rx] Simplex.ring;
}

(* A connect consists of a single handshake "packet" that
 * contains data with handshake information, and two file
 * descriptors: the first is a metadata pipe, and the second
 * is the shared memory descriptor of the connector's transmit
 * channel.
 * The connector will then listen on the supplied metadata socket
 * for the response handshake, which will echo the name and
 * supply a single fd of the receiver's transmit channel.
 *)
type handshake_client = {
  hc_name: string;
  hc_tx_len: int;
} and handshake_server = {
  hs_name: string;
  hc_rx_len: int;
}

(* TODO dont put in cwd *)
let socketpath ~name =
  sprintf "fable-%s.sock" name

let make_recv_ivs () =
  let length = 8192 in
  let buffer = String.create length in
  [Lwt_unix.io_vector ~buffer ~offset:0 ~length], buffer

let make_send_ivs v =
  let buffer = Marshal.to_string v [] in
  let length = String.length buffer in
  [Lwt_unix.io_vector ~buffer ~offset:0 ~length]
  
(* Allocate a shared memory fd of a given length *)
let alloc_shm len =
  let shmfd = Shm.open_anonymous () in
  let fd = Shm.unix_descr_of_shm shmfd in
  let ufd = Lwt_unix.of_unix_file_descr ~blocking:true ~set_flags:false fd in
  lwt () = Lwt_unix.ftruncate ufd len in
  return shmfd

(* A listening service sits on a well-known named socket and
 * waits for requests for a new duplex channel to come over it *)
let listen ~name fn =
  let recv_buf = 32768 in
  let sockaddr = Lwt_unix.ADDR_UNIX (socketpath ~name) in
  let fd = Lwt_unix.(socket PF_UNIX SOCK_DGRAM 0) in
  Lwt_unix.bind fd sockaddr;
  let rec listen_t () =
    let io_vectors, buffer = make_recv_ivs () in
    lwt d, fds = Lwt_unix.recv_msg ~socket:fd ~io_vectors in
    let client_h = Marshal.from_string buffer 0 in
    eprintf "accept: name=%s tx_len=%d\n%!" client_h.hc_name client_h.hc_tx_len;
    let md, shm =
     match fds with
     |[md;shm] -> (Lwt_unix.of_unix_file_descr ~blocking:false md), (Shm.shm_of_unix_descr shm) 
     |_ -> assert false 
    in
    (* Attach the receive end of the metadata ring *)
    let send_ring_rx = Simplex.attach_rx shm client_h.hc_tx_len in
    (* Allocate a transmit ring for our side *)
    lwt recv_fd = alloc_shm recv_buf in
    let recv_ring_tx = Simplex.attach_tx recv_fd recv_buf in
    (* Respond with the server handshake over the metadata socket *)
    let handshake_s = { hs_name=client_h.hc_name; hc_rx_len=recv_buf } in
    let io_vectors = make_send_ivs handshake_s in
    lwt _ = Lwt_unix.send_msg ~socket:md ~io_vectors ~fds:[Shm.unix_descr_of_shm recv_fd] in
    (* Launch the receive function asynchronously with the channel descr *)
    let _ = fn {name=client_h.hc_name; tx=recv_ring_tx; rx=send_ring_rx; fd=md } in
    listen_t ()
  in
  listen_t ()

(* Connect to the listening socket and establish a duplex channel *)
let connect ~name () =
  let sockaddr = Lwt_unix.ADDR_UNIX (socketpath ~name) in
  let nr_pages = 8 in
  let nr_bytes = nr_pages * 4096 in
  let fd = Lwt_unix.(socket PF_UNIX SOCK_DGRAM 0) in
  let buffer = Marshal.to_string nr_bytes [] in
  let iv = Lwt_unix.io_vector ~buffer ~offset:0 ~length:(String.length buffer) in
  (* connect to the socket and send the shmem fd *)
  lwt () = Lwt_unix.connect fd sockaddr in
  (* make an shm fd *)
  let hc_name = "conn_name" in
  lwt shmfd = alloc_shm nr_bytes in
  let send_ring_tx = Simplex.attach_tx shmfd nr_bytes in
  (* allocate a socket pair for metadata *)
  let md1, md2 = Lwt_unix.(socketpair PF_UNIX SOCK_STREAM 0) in
  (* send the handshake message to the listen socket *)
  let client_h = { hc_name; hc_tx_len=nr_bytes } in
  let fds = [ (Lwt_unix.unix_file_descr md2); (Shm.unix_descr_of_shm shmfd) ] in
  let io_vectors = make_send_ivs client_h in
  lwt _ = Lwt_unix.send_msg ~socket:fd ~io_vectors ~fds in 
  (* XXX close md2 here *)
  (* receive the response from the metadata socket *)
  let io_vectors, buffer = make_recv_ivs () in
  lwt d, fds = Lwt_unix.recv_msg ~socket:md1 ~io_vectors in
  let server_h = Marshal.from_string buffer 0 in
  eprintf "connect got server_h: name %s\n%!" server_h.hs_name;
  let recv_fd = match fds with |[fd] -> Shm.shm_of_unix_descr fd |_ -> assert false in
  let recv_ring_rx = Simplex.attach_rx recv_fd server_h.hc_rx_len in
  let ch = { name=server_h.hs_name; tx=send_ring_tx; rx=recv_ring_rx; fd=md1 } in
  eprintf "connect done\n%!";
  return ()
