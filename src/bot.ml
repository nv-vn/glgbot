open Api

open Lwt
open Cohttp
open Cohttp_lwt_unix

open Yojson.Safe

(* Remember, we start from _build/ *)
let bot_token = [%blob "../bot.token"]

let bot_url = "https://api.telegram.org/bot" ^ bot_token ^ "/"

let call fn =
  let url = bot_url ^ fn in
  Client.get (Uri.of_string url) >>= fun (resp, body) ->
  Cohttp_lwt_body.to_string body

let get_updates =
  call "getUpdates" >>= fun json ->
  return @@ Api.Response.read (from_string json)

let format_update update =
  let open Api.Update  in
  let open Api.Message in
  let open Api.User    in
  match update.message with
  | Some message -> begin
      let text = match message.text with
        | Some text -> "Got '" ^ text ^ "'"
        | None -> "Non-text message received" in
      let from = match message.from with
        | Some user -> user.first_name
        | None -> "unknown user" in
      text ^ " from " ^ from
    end
  | None -> "No message included"

let () =
  let body = Lwt_main.run @@ call "getMe" in
  print_endline body;
  match Lwt_main.run get_updates with
  | Updates msgs -> List.iter (fun update -> print_endline @@ format_update update) msgs
  | _ -> print_endline "Error while reading message queue"
