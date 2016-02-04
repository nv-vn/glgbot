open Lwt
open Cohttp
open Cohttp_lwt_unix

open Yojson.Safe

module MyBot = Api.Mk (struct
    open Api.Command

    let token = [%blob "../bot.token"] (* Remember, we start from _build/ *)
    let commands = [{name = "hello"; run = (fun args -> print_endline "Hello, world")}]
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
  Lwt_main.run @@ MyBot.pop_update ();
  Lwt_main.run
    begin
      MyBot.get_updates >>= fun updates ->
      match updates with
      | Success msgs -> begin
        List.fold_right (fun msg cont -> echo msg >>= fun _ -> cont) msgs (return ())
        end
      | Failure error -> Lwt.return @@ print_endline error
    end
