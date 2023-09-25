open Core
open Core_unix

let buffsize = 1024
let port = 8080

let rec echo_data client_sock =
  let buf = Bytes.create buffsize in
  let n = read client_sock ~buf ~pos:0 ~len:buffsize in
  if n = 0 then ()
  else (
    write client_sock ~buf ~pos:0 ~len:n |> ignore;
    echo_data client_sock)

let main () =
  let addr = ADDR_INET (Inet_addr.bind_any, port) in
  let sock = socket ~domain:PF_INET ~kind:SOCK_STREAM ~protocol:0 () in
  setsockopt sock SO_REUSEADDR true;

  bind sock ~addr;
  listen sock ~backlog:5;
  Printf.printf "Listening on port %d\n" port;

  while true do
    let client_sock, _ = accept sock in
    Printf.printf "Accepted connection\n";
    match fork () with
    | `In_the_child ->
        echo_data client_sock;
        close client_sock;
        exit 0
    | `In_the_parent _ -> close client_sock
  done

let () = main ()
