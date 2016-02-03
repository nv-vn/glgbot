open Lwt
open Cohttp
open Cohttp_lwt_unix

(* Remember, we start from _build/ *)
let bot_token = [%blob "../bot.token"]

let bot_url = "https://api.telegram.org/bot" ^ bot_token ^ "/"

let call fn =
  let url = bot_url ^ fn in
  Client.get (Uri.of_string url) >>= fun (resp, body) ->
    Cohttp_lwt_body.to_string body

let () =
  let body = Lwt_main.run @@ call "getMe" in
  print_endline body
