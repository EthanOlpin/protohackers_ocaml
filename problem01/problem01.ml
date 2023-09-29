open Core

type request =
  { method_ : string
  ; number : Z.t
  }

let filter_z l =
  List.filter_map l ~f:(function
    | `Float f -> Some (Z.of_float f)
    | `Intlit s -> Some (Z.of_string s)
    | `Int i -> Some (Z.of_int i)
    | _ -> None)
;;

let to_method_number_opt json =
  let open Yojson.Safe.Util in
  let list = [ json ] in
  let method_opt = list |> filter_member "method" |> filter_string |> List.hd in
  let num_opt = list |> filter_member "number" |> filter_z |> List.hd in
  match method_opt, num_opt with
  | Some m, Some n -> Some { method_ = m; number = n }
  | _ -> None
;;

let is_prime n =
  let open Z in
  let open Poly in
  let rec loop x = x * x > n || (n mod x <> Z.zero && loop (x + Z.one)) in
  n >= Z.of_int 2 && loop (Z.of_int 2)
;;

let write_string flow str =
  Eio.Buf_write.with_flow flow
  @@ fun writer ->
  let line = str ^ "\n" in
  Eio.Buf_write.string writer line;
  Printf.printf "Wrote message: %s\n%!" str
;;

let reject flow =
  let json = `Assoc [] in
  let msg = Yojson.Safe.to_string json in
  write_string flow msg
;;

let accept flow (meth, num) =
  let json = `Assoc [ "method", `String meth; "prime", `Bool (is_prime num) ] in
  let msg = Yojson.Safe.to_string json in
  write_string flow msg
;;

let handle flow = function
  | Some { method_; number } -> accept flow (method_, number)
  | _ -> reject flow
;;

let handle_session flow _ =
  let open Eio.Buf_read in
  let reader = of_flow flow ~max_size:1_000_000 in
  let rec loop () =
    match line reader with
    | line ->
      Printf.printf "Read message: %s\n%!" line;
      (match Yojson.Safe.from_string line with
       | json -> to_method_number_opt json |> handle flow
       | exception Yojson.Json_error _ -> reject flow);
      loop ()
    | exception End_of_file -> ()
  in
  loop ()
;;

let main () =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let socket =
    Eio.Net.listen
      ~reuse_addr:true
      ~backlog:5
      ~sw
      net
      (`Tcp (Eio.Net.Ipaddr.V4.any, 8080))
  in
  while true do
    Eio.Net.accept_fork ~sw socket ~on_error:raise handle_session
  done
;;

let () = main ()
