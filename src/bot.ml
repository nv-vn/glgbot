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
      let quote = function (* FIXME: Telegram API is seemingly broken... *)
        | {chat; reply_to_message = Some ({text = Some text} as msg)} ->
          SendMessage (chat.id, "Quoting " ^ get_sender msg ^ " who said:\n" ^ text)
        | {chat; reply_to_message = Some msg} ->
          SendMessage (chat.id, "Quoting " ^ get_sender msg ^ " who said nothing")
        | {chat} -> SendMessage (chat.id, "Nobody to quote!") in
      let decide = function
        | {chat; text = Some text} ->
          let options = Api.Command.tokenize text in
          let len = List.length options in
          if len = 0 then
            SendMessage (chat.id, "Give me options nerd")
          else if len = 1 then
            SendMessage (chat.id, List.nth ["yes"; "no"] (Random.int 2))
          else
            SendMessage (chat.id, List.nth options (Random.int len)) in
      let share_audio song performer title = function
        | {chat; message_id} -> ResendAudio (chat.id, song, performer, title, Some message_id) in
      let free' = function
        | {chat} -> SendVoice (chat.id, "data/free.ogg", None, function Success id -> SendMessage (chat.id, "That file's ID is " ^ id)
                                                                      | Failure er -> SendMessage (chat.id, "Failed to send audio with: " ^ er)) in
      [{name = "hello"; description = "Greet the user"; run = greet};
       {name = "free"; description = "Free the world from the clutches of proprietary software"; run = share_audio "BQADAQADcQADi_LrCWoG5Wp27N76Ag" "Richard M. Stallman" "Free Software Song"};
       {name = "ocaml"; description = "Shill OCaml"; run = share_audio "BQADAQADcgADi_LrCdarRiXyyEZbAg" "Nate Foster" "flOCaml"};
       {name = "unfree"; description = "Testing voice API"; run = free'};
       {name = "q"; description = "Save a quote"; run = quote};
       {name = "decide"; description = "Help make a decision"; run = decide}]
end)

type 'a result = 'a Api.Result.result

let _ =
  let open Api.Result in
  let open Api.Update in
  let open Api.User in
  let body = (fun user -> user.first_name) <$> Lwt_main.run MyBot.get_me in
  print_endline @@ default "Error on call to `getMe`!" body;
  let open Lwt in
  let process = function
    | Success _ -> return ()
    | Failure e ->
      if e <> "Could not get head" then (* Ignore the huge fucking command line spam *)
        Lwt_io.printl e
      else return () in
  let rec loop () =
    MyBot.pop_update () >>= process >>= loop in
  Lwt_main.run @@ loop ()
