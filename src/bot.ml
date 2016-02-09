open Lwt
open Cohttp
open Cohttp_lwt_unix

open Yojson.Safe

module MyBot = Api.Mk (struct
    open Api.Chat
    open Api.Audio
    open Api.Result
    open Api.Update
    open Api.Command
    open Api.Message
    open Api.Audio.Out

    let token = [%blob "../bot.token"] (* Remember, we start from _build/ *)
    let commands =
      let open Lwt in
      let greet = function
        | {text = Some text; chat} as msg -> SendMessage (chat.id, "Hello, " ^ get_sender msg)
        | _ -> Nothing in
      let quote = function
        | {chat; reply_to_message = Some ({text = Some text} as msg)} ->
          Db.Quotes.put ~quote:text ~who:(get_sender msg) ();
          SendMessage (chat.id, "Quoting " ^ get_sender msg ^ " who said:\n" ^ text)
        | {chat; reply_to_message = Some msg} ->
          SendMessage (chat.id, "Quoting " ^ get_sender msg ^ " who said nothing")
        | {chat} ->
          let (sender, msg, time) = Db.Quotes.get_random () in
          SendMessage (chat.id, sender ^ " said:\n" ^ msg) in
      let decide = function
        | {chat; text = Some text} ->
          let open Batteries.String in
          let all = List.filter ((<>) "") @@ List.map strip @@ Api.Command.tokenize text in
          let rec get_options buf = function
            | [] -> buf
            | [a] -> a::buf
            | a::"or"::b -> get_options (a::buf) b
            | a::b::c -> get_options buf ((a ^ " " ^ b)::c) in
          let options = get_options [] all in
          let len = List.length options in
          if len = 0 then
            SendMessage (chat.id, "Give me options nerd")
          else if len = 1 then
            SendMessage (chat.id, List.nth ["yes"; "no"] (Random.int 2))
          else
            SendMessage (chat.id, List.nth options (Random.int len))
        | _ -> Nothing in
      let jukebox = function
        | {chat; text = Some text; reply_to_message = Some {audio = Some {file_id}}} -> begin
            let open Batteries.String in
            let (_, info) = split text ~by:" " in
            let (performer, title) = split info ~by:"-" in
            Db.Jukebox.put ~title:(trim @@ title) ~performer ~file_id ();
            SendMessage (chat.id, "Added " ^ info ^ " to the database!")
          end
        | {chat; message_id; text = Some text} -> begin
            let open Batteries.String in
            match tokenize text with
            | [] -> SendMessage (chat.id, concat "\n" (Db.Jukebox.list ()))
            | xs -> begin
                let song = trim @@ concat " " xs in
                let (title, performer, file_id) = Db.Jukebox.search ~title:song in
                ResendAudio (chat.id, file_id, performer, title, Some message_id)
              end
          end
        | {chat} -> SendMessage (chat.id, "Invalid input") in
      let share_audio song performer title = function
        | {chat; message_id} -> ResendAudio (chat.id, song, performer, title, Some message_id) in
      let unfree = function
        | {chat} -> SendVoice (chat.id, "data/free.ogg", None, function Success id -> SendMessage (chat.id, "That file's ID is " ^ id)
                                                                      | Failure er -> SendMessage (chat.id, "Failed to send audio with: " ^ er)) in
      [{name = "hello"; description = "Greet the user"; run = greet};
       {name = "free"; description = "Free the world from the clutches of proprietary software"; run = share_audio "BQADAQADcQADi_LrCWoG5Wp27N76Ag" "Richard M. Stallman" "Free Software Song"};
       {name = "ocaml"; description = "Shill OCaml"; run = share_audio "BQADAQADcgADi_LrCdarRiXyyEZbAg" "Nate Foster" "flOCaml"};
       {name = "dab"; description = "Pipe it up"; run = share_audio "BQADAQADcwADi_LrCbRvyK66JIVTAg" "Migos" "Pipe It Up"};
       {name = "unfree"; description = "Testing voice API"; run = unfree};
       {name = "q"; description = "Save a quote"; run = quote};
       {name = "jukebox"; description = "Store and play music"; run = jukebox};
       {name = "decide"; description = "Help make a decision"; run = decide}]
end)

type 'a result = 'a Api.Result.result

let rec main () =
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
  try Lwt_main.run @@ loop ()
  with Unix.Unix_error (_, _, _) -> main ()

let _ = main ()
