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
      let greet = function
        | {text = Some text; chat} -> SendMessage (chat.id, "Hello")
        | _ -> Nothing in
      let help = function
        | {chat} -> Chain (SendMessage (chat.id, "Commands:"),
                           Chain (SendMessage (chat.id, "/help - Show this message"),
                                  SendMessage (chat.id, "/hello - Greet the user"))) in
      [{name = "hello"; run = greet};
       {name = "help"; run = help}]
end)

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

let echo update =
  let open Api.Update  in
  let open Api.Message in
  let open Api.User    in
  let open Api.Chat    in
  match update.message with
  | Some message -> begin
      let text = match message.text with
        | Some text -> text
        | None -> "Please send a valid message"
      and chat = message.chat.id in
      MyBot.send_message ~chat_id:chat ~text
    end
  | None -> return @@ Api.Result.Failure "Can't read message to echo"

type 'a result = 'a Api.Result.result

let () =
  let open Api.Result in
  let open Api.User in
  let body = (fun user -> user.first_name) <$> Lwt_main.run MyBot.get_me in
  print_endline @@ default "Error on call to `getMe`!" body;
  let open Lwt in
  while true do
    try Lwt_main.run @@ MyBot.pop_update ()
    with _ -> Success {update_id = 0; message = None}
  done
