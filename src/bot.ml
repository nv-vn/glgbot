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
        | {chat; message_id} -> ResendAudio (chat.id, song, performer, title, Some message_id) in
      let free' = function
        | {chat} -> SendVoice (chat.id, "data/free.ogg", None, function Success id -> return @@ print_endline id) in
      [{name = "hello"; description = "Greet the user"; run = greet};
       {name = "free"; description = "Free the world from the clutches of proprietary software"; run = share_audio "BQADAQADcQADi_LrCWoG5Wp27N76Ag" "Richard M. Stallman" "Free Software Song"};
       {name = "ocaml"; description = "Shill OCaml"; run = share_audio "BQADAQADcgADi_LrCdarRiXyyEZbAg" "Nate Foster" "flOCaml"};
       {name = "unfree"; description = "Testing voice API"; run = free'}]
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
