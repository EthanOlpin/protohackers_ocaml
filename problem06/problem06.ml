open Core
open Eio.Std

let port = 8080

type client =
  | Camera of
      { road : int
      ; mile : int
      ; speed_limit : int
      }
  | Dispatcher
  | Unknown

type ticket =
  { plate : string
  ; road : int
  ; mile1 : int
  ; timestamp1 : int32
  ; mile2 : int
  ; timestamp2 : int32
  ; speed : int
  }

type server_message =
  | Error of string
  | Ticket of ticket
  | Heartbeat

let server_message_code = function
  | Error _ -> 0x10
  | Ticket _ -> 0x21
  | Heartbeat -> 0x41
;;

type client_message =
  | Plate of
      { plate : string
      ; timestamp : int32
      }
  | WantHeartbeat of int32
  | IAmCamera of
      { road : int
      ; mile : int
      ; speed_limit : int
      }
  | IAmDispatcher of
      { num_roads : int
      ; roads : int list
      }
  | Invalid of int

type plate_record =
  { road : int
  ; mile : int
  ; timestamp : int32
  ; speed_limit : int
  }

let write_string writer s =
  let open Eio.Buf_write in
  let len = String.length s in
  uint8 writer len;
  string writer s
;;

let read_string reader =
  let open Eio.Buf_read in
  let len = uint8 reader in
  take len reader
;;

let write_server_message with_writer msg =
  let open Eio.Buf_write in
  with_writer (fun writer ->
    uint8 writer (server_message_code msg);
    match msg with
    | Error s ->
      write_string writer s;
      Printf.printf "[SERVER] Error %s\n%!" s
    | Ticket { plate; road; mile1; timestamp1; mile2; timestamp2; speed } ->
      Printf.printf
        "[SERVER] Ticket { plate = %s; road = %d; mile1 = %d; timestamp1 = \
         %ld; mile2 = %d; timestamp2 = %ld; speed = %d }\n\
         %!"
        plate
        road
        mile1
        timestamp1
        mile2
        timestamp2
        speed;
      write_string writer plate;
      BE.uint16 writer road;
      BE.uint16 writer mile1;
      BE.uint32 writer timestamp1;
      BE.uint16 writer mile2;
      BE.uint32 writer timestamp2;
      BE.uint16 writer speed
    | Heartbeat -> ())
  |> ignore
;;

let read_client_message reader =
  let open Eio.Buf_read in
  let code = uint8 reader in
  let msg =
    match code with
    | 0x20 ->
      let plate = read_string reader in
      let timestamp = BE.uint32 reader in
      Printf.printf
        "[CLIENT] Plate { plate = %s; timestamp = %ld }\n%!"
        plate
        timestamp;
      Plate { plate; timestamp }
    | 0x40 ->
      let interval = BE.uint32 reader in
      Printf.printf "[CLIENT] WantHeartbeat %ld\n%!" interval;
      WantHeartbeat interval
    | 0x80 ->
      let road = BE.uint16 reader in
      let mile = BE.uint16 reader in
      let speed_limit = BE.uint16 reader in
      Printf.printf
        "[CLIENT] IAmCamera { road = %d; mile = %d; speed_limit = %d }\n%!"
        road
        mile
        speed_limit;
      IAmCamera { road; mile; speed_limit }
    | 0x81 ->
      let num_roads = uint8 reader in
      let roads = List.init num_roads ~f:(fun _ -> BE.uint16 reader) in
      Printf.printf
        "[CLIENT] IAmDispatcher { num_roads = %d; roads = %s }\n%!"
        num_roads
        (List.to_string ~f:Int.to_string roads);
      IAmDispatcher { num_roads; roads }
    | _ -> Invalid code
  in
  msg
;;

let start_heartbeat ~sw clock with_writer interval_s =
  let rec heartbeat () =
    write_server_message with_writer Heartbeat;
    let interval_decis = Int32.to_float interval_s /. 10. in
    Eio.Time.sleep clock interval_decis;
    heartbeat ()
  in
  Fiber.fork ~sw (fun () -> heartbeat ())
;;

let compute_mph timestamp1 mile1 timestamp2 mile2 =
  let time = Int32.(to_float (abs (timestamp2 - timestamp1)) /. 3600.) in
  let distance = Float.abs (Float.of_int (mile2 - mile1)) in
  distance /. time
;;

let get_day timestamp = Int32.(to_int_trunc (timestamp / 86_400l))

let generate_tickets plate records =
  let sorted_records =
    List.sort records ~compare:(fun a b ->
      Int32.compare a.timestamp b.timestamp)
  in
  let rec loop records tickets =
    match records with
    | [] | [ _ ] -> tickets
    | record1 :: record2 :: records ->
      let mph =
        compute_mph
          record1.timestamp
          record1.mile
          record2.timestamp
          record2.mile
      in
      if Float.(mph - of_int record2.speed_limit >= 0.5)
      then (
        let ticket =
          { plate
          ; road = record1.road
          ; mile1 = record1.mile
          ; timestamp1 = record1.timestamp
          ; mile2 = record2.mile
          ; timestamp2 = record2.timestamp
          ; speed = Float.(to_int (round_nearest mph)) * 100
          }
        in
        loop (record2 :: records) (ticket :: tickets))
      else loop (record2 :: records) tickets
  in
  loop sorted_records []
;;

module TicketPool = struct
  type t =
    { ticket_queues : ticket Eio.Stream.t Int.Table.t
    ; mutex : Eio.Mutex.t
    }

  let create () =
    { ticket_queues = Int.Table.create (); mutex = Eio.Mutex.create () }
  ;;

  let get_ticket_queue t road =
    Eio.Mutex.use_rw t.mutex ~protect:false
    @@ fun () ->
    Hashtbl.find_or_add t.ticket_queues road ~default:(fun () ->
      Eio.Stream.create 100)
  ;;

  let enqueue_ticket t road ticket =
    let stream = get_ticket_queue t road in
    Eio.Stream.add stream ticket
  ;;
end

module DB = struct
  type t =
    { plate_ticketed_days : int Hash_set.t String.Table.t
    ; plate_data : plate_record list String.Table.t
    ; plate_ticketed_days_mutex : Eio.Mutex.t
    ; plate_data_mutex : Eio.Mutex.t
    }

  let create () =
    { plate_ticketed_days = String.Table.create ()
    ; plate_data = String.Table.create ()
    ; plate_ticketed_days_mutex = Eio.Mutex.create ()
    ; plate_data_mutex = Eio.Mutex.create ()
    }
  ;;

  let get_plate_ticket_days t plate =
    Eio.Mutex.use_rw t.plate_ticketed_days_mutex ~protect:false
    @@ fun () ->
    Hashtbl.find_or_add t.plate_ticketed_days plate ~default:(fun () ->
      Hash_set.create (module Int))
  ;;

  let insert_plate_ticket_day t plate day =
    let days = get_plate_ticket_days t plate in
    Eio.Mutex.use_rw t.plate_ticketed_days_mutex ~protect:false (fun () ->
      Hash_set.add days day)
  ;;

  let plate_was_ticketed_on_day t plate day =
    let days = get_plate_ticket_days t plate in
    Hash_set.mem days day
  ;;

  let get_plate_records t plate =
    Eio.Mutex.use_rw t.plate_data_mutex ~protect:false
    @@ fun () -> Hashtbl.find_or_add t.plate_data plate ~default:(fun () -> [])
  ;;

  let set_plate_records t plate records =
    Eio.Mutex.use_rw t.plate_data_mutex ~protect:false
    @@ fun () -> Hashtbl.set t.plate_data ~key:plate ~data:records
  ;;

  let insert_plate_record t plate record =
    let records = get_plate_records t plate in
    set_plate_records t plate (record :: records)
  ;;

  let get_applicable_tickets t plate =
    let records = get_plate_records t plate in
    let possible_tickets = generate_tickets plate records in
    let tickets_days =
      List.map possible_tickets ~f:(fun ticket ->
        let day1 = get_day ticket.timestamp1 in
        let day2 = get_day ticket.timestamp2 in
        ticket, day1, day2)
    in
    List.fold tickets_days ~init:[] ~f:(fun tickets (ticket, day1, day2) ->
      if plate_was_ticketed_on_day t plate day1
         || plate_was_ticketed_on_day t plate day2
      then tickets
      else (
        insert_plate_ticket_day t plate day1;
        insert_plate_ticket_day t plate day2;
        ticket :: tickets))
  ;;
end

let listen_for_tickets_on_road ticket_pool with_writer road =
  let rec loop () =
    let stream = TicketPool.get_ticket_queue ticket_pool road in
    let ticket = Eio.Stream.take stream in
    write_server_message with_writer (Ticket ticket);
    loop ()
  in
  loop ()
;;

let listen_for_tickets_on_roads ~sw ticket_pool roads with_writer =
  Fiber.fork ~sw (fun () ->
    Fiber.List.iter
      (fun road -> listen_for_tickets_on_road ticket_pool with_writer road)
      roads)
;;

let listen ~sw ~clock ~db ~ticket_pool flow _ =
  let reader = Eio.Buf_read.of_flow flow ~max_size:1_000_000 in
  let with_writer f =
    try Eio.Buf_write.with_flow flow f with
    | _ -> ()
  in
  let rec loop client ~has_heartbeat =
    match client, read_client_message reader with
    | Unknown, IAmCamera { road; mile; speed_limit } ->
      loop (Camera { road; mile; speed_limit }) ~has_heartbeat
    | Unknown, IAmDispatcher { num_roads = _; roads } ->
      listen_for_tickets_on_roads ~sw ticket_pool roads with_writer;
      loop Dispatcher ~has_heartbeat
    | Camera { road; mile; speed_limit }, Plate { plate; timestamp } ->
      DB.insert_plate_record db plate { road; mile; timestamp; speed_limit };
      let applicable_tickets = DB.get_applicable_tickets db plate in
      List.iter applicable_tickets ~f:(fun ticket ->
        TicketPool.enqueue_ticket ticket_pool ticket.road ticket);
      loop client ~has_heartbeat
    | _, WantHeartbeat interval when not has_heartbeat ->
      if Int32.(interval > zero)
      then start_heartbeat ~sw clock with_writer interval
      else ();
      loop client ~has_heartbeat:true
    | _, WantHeartbeat _ ->
      write_server_message with_writer (Error "Already sending heartbeats")
    | _, Plate _ ->
      write_server_message
        with_writer
        (Error "Received plate but client did not identify as camera")
    | _, IAmCamera _ | _, IAmDispatcher _ ->
      write_server_message with_writer (Error "Already identified")
    | _, Invalid code ->
      let error_msg = Printf.sprintf "Invalid message with code %#x" code in
      write_server_message with_writer (Error error_msg)
    | exception _ -> ()
  in
  loop Unknown ~has_heartbeat:false
;;

let main () =
  Eio_main.run
  @@ fun env ->
  Switch.run
  @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in
  let client_addr = `Tcp (Eio.Net.Ipaddr.V4.any, port) in
  let db = DB.create () in
  let ticket_pool = TicketPool.create () in
  let socket =
    Eio.Net.listen ~reuse_addr:true ~sw ~backlog:100 net client_addr
  in
  while true do
    Eio.Net.accept_fork
      ~sw
      socket
      ~on_error:raise
      (listen ~sw ~clock ~db ~ticket_pool)
  done
;;

let () = main ()
