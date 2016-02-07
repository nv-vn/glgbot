open Lwt
open Cohttp
open Cohttp_lwt_unix

open Yojson.Safe

module MyBot = Api.Mk (struct
    open Api.Chat
    open Api.Result
    open Api.Command
    open Api.Message

    let token = [%blob "../bot.token"] (* Remember, we start from _build/ *)
    let commands =
      let open Lwt in
      let greet = function
        | {text = Some text; chat} as msg -> SendMessage (chat.id, "Hello, " ^ get_sender msg)
        | _ -> Nothing in
      let share_audio song performer title = function
        | {chat; message_id} -> SendAudio (chat.id, song, performer, title, Some message_id) in
      let free' = function
        | {chat} -> begin
            begin
              let url = "https://api.telegram.org/bot" ^ token ^ "/" in
              let boundary = "---1234567890" in
              let headers = Cohttp.Header.init_with "Content-Type" ("multipart/form-data; boundary=" ^ boundary) in
              Api.InputFile.multipart_body ["chat_id", string_of_int chat.id] ("voice", "data/free.ogg", "audio/ogg") boundary >>= fun body_text ->
              let body = Cohttp_lwt_body.of_string body_text in
              Client.post ~headers ~body (Uri.of_string (url ^ "sendVoice")) >>= fun (resp, body) ->
              Cohttp_lwt_body.to_string body >>= fun json ->
              return @@ print_endline json
            end |> Lwt_main.run;
            Nothing
          end in
      [{name = "hello"; description = "Greet the user"; run = greet};
       {name = "free"; description = "Free the world from the clutches of proprietary software"; run = share_audio "BQADAQADcQADi_LrCWoG5Wp27N76Ag" "Richard M. Stallman" "Free Software Song"};
       {name = "ocaml"; description = "Shill OCaml"; run = share_audio "BQADAQADcgADi_LrCdarRiXyyEZbAg" "Nate Foster" "flOCaml"};
       {name = "unfree"; description = "Testing voice API"; run = free'}]
end)

type 'a result = 'a Api.Result.result

let () =
  let open Api.Result in
  let open Api.Update in
  let open Api.User in
  let body = (fun user -> user.first_name) <$> Lwt_main.run MyBot.get_me in
  print_endline @@ default "Error on call to `getMe`!" body;
  let open Lwt in
  while true do
    try ignore @@ Lwt_main.run @@ MyBot.pop_update ()
    with _ -> ()
  done
