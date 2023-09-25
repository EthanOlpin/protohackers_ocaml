open Core
open Eio

let port = 8080

module ClientData = struct
  type t = { mutable prices : (int * int) list }

  let create () = { prices = [] }
  let insert t timestamp price = t.prices <- (timestamp, price) :: t.prices

  let query t mintime maxtime =
    let total, count =
      List.filter t.prices ~f:(fun (timestamp, _) ->
          mintime <= timestamp && timestamp <= maxtime)
      |> List.fold ~init:(0, 0) ~f:(fun (total, count) (_, price) ->
             (total + price, count + 1))
    in
    if count = 0 then 0 else total / count
end

type message =
  | Insert of { timestamp : int; price : int }
  | Query of { mintime : int; maxtime : int }

let read_message reader =
  let msg_type = Buf_read.any_char reader in
  let x1 = Buf_read.BE.uint32 reader |> Int.of_int32_exn in
  let x2 = Buf_read.BE.uint32 reader |> Int.of_int32_exn in
  Printf.printf "Read message: %c %d %d\n%!" msg_type x1 x2;
  match msg_type with
  | 'I' -> Insert { timestamp = x1; price = x2 }
  | 'Q' -> Query { mintime = x1; maxtime = x2 }
  | _ -> failwith "Invalid message type"

let write_message flow msg =
  Buf_write.with_flow flow @@ fun writer ->
  Buf_write.BE.uint32 writer (Int32.of_int_exn msg);
  Printf.printf "Wrote message: %d\n%!" msg

let handle_session flow _ =
  let client_data = ClientData.create () in
  let reader = Buf_read.of_flow flow ~max_size:1_000_000 in
  let rec loop () =
    match read_message reader with
    | Insert { timestamp; price } ->
        ClientData.insert client_data timestamp price;
        loop ()
    | Query { mintime; maxtime } ->
        let mean = ClientData.query client_data mintime maxtime in
        write_message flow mean;
        loop ()
    | (exception End_of_file) | (exception Failure _) -> ()
  in
  loop ()

let main () =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let net = Stdenv.net env in
  let socket =
    Net.listen ~reuse_addr:true ~backlog:5 ~sw net
      (`Tcp (Net.Ipaddr.V4.any, port))
  in
  while true do
    Net.accept_fork ~sw socket ~on_error:raise handle_session
  done

let () = main ()
