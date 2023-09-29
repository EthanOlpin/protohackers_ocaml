open Core
open Eio.Std

let port = 8080

module DB = struct
  type t = { data : (string, string) Hashtbl.t }

  let create () = { data = Hashtbl.create (module String) }

  let get t key =
    match key with
    | "version" -> Some "4.2.0"
    | _ -> Hashtbl.find t.data key
  ;;

  let set t k v = Hashtbl.set t.data ~key:k ~data:v
end

let recieve_on db socket =
  let buffer = Cstruct.create 1024 in
  let from, len = Eio.Net.recv socket buffer in
  let message = Cstruct.to_string ~len buffer in
  match String.lsplit2 message ~on:'=' with
  | None ->
    let value = DB.get db message |> Option.value ~default:"" in
    let response = [ Cstruct.of_string (message ^ "=" ^ value) ] in
    Eio.Net.send socket ~dst:from response
  | Some (key, value) -> DB.set db key value
;;

let main () =
  Eio_main.run
  @@ fun env ->
  Switch.run
  @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let addr = `Udp (Eio.Net.Ipaddr.V4.any, port) in
  let socket = Eio.Net.datagram_socket ~reuse_addr:true ~sw net addr in
  let db = DB.create () in
  while true do
    recieve_on db socket
  done
;;

let () = main ()
