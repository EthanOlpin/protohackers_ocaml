open Core
open Eio.Std

let port = 8080
let boguscoin_replacement_address = "7YWHMfk9JZe0LM0g1ZauHuiSxhI"
let boguscoin_address_re = Re.Pcre.regexp {|^(7[a-zA-Z0-9]{25,34})$|}

let replace_boguscoin_addresses s =
  let words = String.split ~on:' ' s in
  List.map words ~f:(fun word ->
    if Re.execp boguscoin_address_re word
    then boguscoin_replacement_address
    else word)
  |> String.concat ~sep:" "
;;

let transform_tunnel inbound_flow outbound_flow transform_f =
  let inbound_reader = Eio.Buf_read.of_flow inbound_flow ~max_size:1_000_000 in
  let rec loop () =
    let line = Eio.Buf_read.take_while (Char.( <> ) '\n') inbound_reader in
    match Eio.Buf_read.take 1 inbound_reader with
    | "\n" ->
      let transformed_msg = transform_f line ^ "\n" in
      Eio.Flow.copy_string transformed_msg outbound_flow;
      loop ()
    | (exception End_of_file) | _ -> ()
  in
  loop ()
;;

let handle_connection net downstream_flow _ =
  Switch.run
  @@ fun sw ->
  let upstream_addr =
    match Eio.Net.getaddrinfo_stream net "chat.protohackers.com" with
    | [] -> failwith "no address found for upstream: chat.protohackers.com"
    | `Tcp (addr, _) :: _ -> `Tcp (addr, 16963)
    | _ -> failwith "found unsupported address type for chat.protohackers.com"
  in
  let upstream_flow = Eio.Net.connect ~sw net upstream_addr in
  Fiber.first
    (fun () ->
      transform_tunnel downstream_flow upstream_flow replace_boguscoin_addresses)
    (fun () ->
      transform_tunnel upstream_flow downstream_flow replace_boguscoin_addresses)
;;

let main () =
  Eio_main.run
  @@ fun env ->
  Switch.run
  @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let client_addr = `Tcp (Eio.Net.Ipaddr.V4.any, port) in
  let socket =
    Eio.Net.listen ~reuse_addr:true ~sw ~backlog:10 net client_addr
  in
  while true do
    Eio.Net.accept_fork ~sw socket ~on_error:raise (handle_connection net)
  done
;;

let () = main ()
