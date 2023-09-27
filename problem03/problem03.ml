open Core
open Eio.Std

let port = 8080
let valid_username_re = Str.regexp "^[a-zA-Z0-9]+$"

let write_message flow msg =
  Eio.Buf_write.with_flow flow @@ fun writer ->
  Eio.Buf_write.string writer (msg ^ "\n")

module UserSession = struct
  type 'a t = { username : string; flow : 'a r }

  let create username flow = { username; flow }

  let send_message t msg =
    let flow = t.flow in
    write_message flow msg
end

module Room = struct
  type 'a t = { mutable users : 'a UserSession.t list }

  let broadcast t ?skip msg =
    List.iter t.users ~f:(fun u ->
        match skip with
        | Some u' when String.equal u.username u' -> ()
        | _ -> UserSession.send_message u msg)

  let create () = { users = [] }
  let get_room_member_names t = List.map t.users ~f:(fun u -> u.username)

  let add_user t user_session =
    UserSession.send_message user_session
      ("* The room contains:"
      ^ (get_room_member_names t |> String.concat ~sep:", "));
    t.users <- user_session :: t.users;
    let username = user_session.username in
    broadcast t ~skip:username ("* " ^ username ^ " has joined the room")

  let remove_user t username =
    t.users <-
      List.filter t.users ~f:(fun u -> not (String.equal u.username username));
    broadcast t ~skip:username ("* " ^ username ^ " has left the room")

  let handle_user_message t username msg =
    broadcast t ~skip:username (Printf.sprintf "[%s] %s" username msg)
end

let handle_session room flow _ =
  write_message flow "Welcome to the chat server! What should we call you?";
  let reader = Eio.Buf_read.of_flow flow ~max_size:1_000_000 in
  let username = try Eio.Buf_read.line reader with _ -> "" in
  if not (Str.string_match valid_username_re username 0) then ()
  else
    let session = UserSession.create username flow in
    Room.add_user room session;
    let rec loop () =
      try
        let message = Eio.Buf_read.line reader in
        Room.handle_user_message room username message;
        loop ()
      with End_of_file | Failure _ -> Room.remove_user room username
    in
    loop ()

let main () =
  let room = Room.create () in
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let socket =
    Eio.Net.listen ~reuse_addr:true ~backlog:10 ~sw net
      (`Tcp (Eio.Net.Ipaddr.V4.any, port))
  in
  while true do
    Eio.Net.accept_fork ~sw socket ~on_error:ignore (handle_session room)
  done

let () = main ()
