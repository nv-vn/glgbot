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
        | {text = Some text; chat} as msg -> SendMessage (chat.id, "Hello, " ^ get_sender msg)
        | _ -> Nothing in
      let help = function
        | {chat} -> SendMessage (chat.id, "Commands:\n/help - Show this message\n/hello - Greet the user\n/free - Free the world from the clutches of proprietary software") in
      let free = function
        | {chat} -> SendAudio (chat.id, "BQADAQADbwADi_LrCZYT832sBq6qAg") in
      [{name = "hello"; run = greet};
       {name = "help"; run = help};
       {name = "free"; run = free}]
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
