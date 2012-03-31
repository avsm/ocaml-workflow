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
  (* human-readable name for this handle *)
  name: string;
  (* metadata pipe *)
  fd: Lwt_unix.file_descr;
  (* Unidirectional data channels *)
  tx: [`tx] Simplex.ring;
  rx: [`rx] Simplex.ring;
  mutable rx_closed: bool;
}

let dprintf fmt =
  let xfn ch = fprintf ch fmt in
  kfprintf xfn stderr "[%d] " (Unix.getpid())

(* Given a handle, retrieve a pair of input/output streams *)
let make_flow handle =
  (* Buffered metadata channels *)
  let ic = Lwt_io.(of_fd ~buffer_size:65535 ~mode:input handle.fd) in
  let oc = Lwt_io.(of_fd ~buffer_size:65535 ~mode:output handle.fd) in
  (* Listeners waiting for new incoming extents *)
  let rx_waiters = Lwt_sequence.create () in
  (* Listeners waiting for free space on the transmit queue *)
  let tx_waiters = Lwt_sequence.create () in
  (* Buffered sequence of receive segments *)
  let rx_q = Lwt_sequence.create () in
  (* Receive stream handler *)
  let rx_stream = Lwt_stream.from
    (fun () ->
      match Lwt_sequence.take_opt_l rx_q with
      |None -> begin
        match handle.rx_closed with
        |true -> return None
        |false ->
          let t,u = Lwt.task () in
          let node = Lwt_sequence.add_r u rx_waiters in
          Lwt.on_cancel t (fun () -> Lwt_sequence.remove node);
          t
      end
      |Some extent -> return (Some extent)
    )
  in
  (* Write buffer allocator *)
  let rec tx_alloc len =
    match Simplex.alloc handle.tx len with
    |None -> (* Not enough space on the transmit queue, so block *)
       let t,u = Lwt.task () in
       let node = Lwt_sequence.add_r u tx_waiters in
       Lwt.on_cancel t (fun () -> Lwt_sequence.remove node);
       lwt () = t in
       tx_alloc len
    |Some extent -> begin
       (* If there isn't enough room, then release the extent
        * immediately and block until more is free *)
        if Simplex.length extent < len then begin
          let t,u = Lwt.task () in
          let node = Lwt_sequence.add_r u tx_waiters in
          Lwt.on_cancel t (fun () -> Lwt_sequence.remove node);
          Simplex.release extent;
          lwt () = t in
          tx_alloc len
        end else
          return extent
    end
  in
  (* Release a local TX buffer transmitter has decided not to use
   * any more *)
  let tx_release ext = Simplex.release ext; return () in
  (* Tx functions to push the correct metadata *)
  let md_write op = try_lwt Lwt_io.write_value oc op with exn -> return () in
  (* XXX notify the transmitter that the receiver has stopped listening, somehow.
   * Right now we just ignore the metadata pipe disappearing *)
  let tx_send extent =
     try_lwt Lwt_io.write_value oc (Simplex.to_send_op extent) >> return true
     with exn -> return false 
  in  
  let tx_close () = md_write (Simplex.to_close_op) in
  let rx_release extent = md_write(Simplex.to_free_op extent) in
  (* The metadata pipe coordinates all this *)
  let _ =
    (* Create a buffered pipe *)
    try_lwt while_lwt true do
      Lwt_io.read_value ic >|= Simplex.on_op 
        ~rx:handle.rx ~tx:handle.tx
        ~send:(fun extent -> (* received a Send *)
           match Lwt_sequence.take_opt_l rx_waiters with
           |None -> ignore(Lwt_sequence.add_r extent rx_q)
           |Some u -> Lwt.wakeup u (Some extent))
        ~free:(fun extent -> (* received a Free *)
           Simplex.release extent;
           match Lwt_sequence.take_opt_l tx_waiters with
           |None -> ()
           |Some u -> Lwt.wakeup u ()) 
        ~close:(fun rx -> (* Rx ring is now Closed *)
           handle.rx_closed <- true;
           Lwt_sequence.iter_l (fun u -> Lwt.wakeup u None) rx_waiters)
    done with exn ->
      handle.rx_closed <- true;
      Lwt_sequence.iter_l (fun u -> Lwt.wakeup u None) rx_waiters;
      return ()
  in
  Lwt_flow.make ~rx_stream ~rx_release ~tx_send ~tx_release ~tx_close ~tx_alloc

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

let make_recv_ivs () =
  let length = 8192 in
  let buffer = String.create length in
  [Lwt_unix.io_vector ~buffer ~offset:0 ~length], buffer

let make_send_ivs v =
  let buffer = Marshal.to_string v [] in
  let length = String.length buffer in
  [Lwt_unix.io_vector ~buffer ~offset:0 ~length]

let listen fd =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let recv_buf = 32768 in
  let io_vectors, buffer = make_recv_ivs () in
  Lwt_stream.from (fun () ->
    try_lwt
      lwt d, fds = Lwt_unix.recv_msg ~socket:fd ~io_vectors in
      let client_h = Marshal.from_string buffer 0 in
      let md, shm =
        match fds with
        |[md;shm] ->
          (Lwt_unix.of_unix_file_descr ~blocking:false ~set_flags:true md),
          (Shm.shm_of_unix_descr shm) 
        |_ -> assert false 
      in
      (* Attach the receive end of the metadata ring *)
      let send_ring_rx = Simplex.attach_rx shm client_h.hc_tx_len in
      (* Allocate a transmit ring for our side *)
      let recv_fd = Shm.open_anonymous () in
      let recv_ring_tx = Simplex.attach_tx recv_fd recv_buf in
      (* Respond with the server handshake over the metadata socket *)
      let server_h = { hs_name=client_h.hc_name; hc_rx_len=recv_buf } in
      let io_vectors = make_send_ivs server_h in
      lwt _ = Lwt_unix.send_msg ~socket:md ~io_vectors ~fds:[Shm.unix_descr_of_shm recv_fd] in
      (* Launch the receive function asynchronously with the channel descr *)
      let h = { name=client_h.hc_name; tx=recv_ring_tx; rx=send_ring_rx; fd=md; rx_closed=false } in
      return (Some h)
     with exn -> 
      return None
  )

(* Connect to the listening socket and establish a duplex channel *)
let connect fd =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let nr_pages = 8 in
  let nr_bytes = nr_pages * 4096 in
  (* connect to the socket and send the shmem fd *)
  (* make an shm fd *)
  let hc_name = "conn_name" in
  let shmfd = Shm.open_anonymous () in
  let send_ring_tx = Simplex.attach_tx shmfd nr_bytes in
  (* allocate a socket pair for metadata *)
  let md1, md2 = Lwt_unix.(socketpair PF_UNIX SOCK_STREAM 0) in
  (* send the handshake message to the listen socket *)
  let client_h = { hc_name; hc_tx_len=nr_bytes } in
  let fds = [ (Lwt_unix.unix_file_descr md2); (Shm.unix_descr_of_shm shmfd) ] in
  let io_vectors = make_send_ivs client_h in
  lwt _ = Lwt_unix.send_msg ~socket:fd ~io_vectors ~fds in 
  Unix.close (Lwt_unix.unix_file_descr fd);
  Unix.close (Lwt_unix.unix_file_descr md2);
  (* XXX odd Lwt close bug here again *)
  (* receive the response from the metadata socket *)
  let io_vectors, buffer = make_recv_ivs () in
  lwt d, fds = Lwt_unix.recv_msg ~socket:md1 ~io_vectors in
  let server_h = Marshal.from_string buffer 0 in
  let recv_fd = match fds with |[fd] -> Shm.shm_of_unix_descr fd |_ -> assert false in
  let recv_ring_rx = Simplex.attach_rx recv_fd server_h.hc_rx_len in
  return { name=server_h.hs_name; tx=send_ring_tx; rx=recv_ring_rx; fd=md1; rx_closed=false } 
