open Lwt
open Yojson.Safe

open Telegram
open TelegramDashboard

module Glg = MkDashboard (struct
    open Api.Chat
    open Api.User
    open Api.Audio
    open Api.Result
    open Api.Update
    open Api.Command
    open Api.Message
    open Api.Audio.Out

    include BotDefaults

    let token = [%blob "../bot.token"] (* Remember, we start from _build/ *)
    let command_postfix = Some "glgbot" (* Ignore commands that have a username other than @glgbot appended *)

    let rec commands =
      let open Lwt in
      let greet = function
        | {chat; message_id} as msg -> SendMessage (chat.id, "Hello, " ^ get_sender msg, false, Some message_id, None) in
      let meow_cache = ref [] in
      let meow = function
        | {chat} -> begin
            if !meow_cache = [] then
              Lwt_main.run (Reddit.get_images ~num:100 "meow_irl" >>= fun images ->
                            meow_cache := images;
                            return ())
            else ();
            let len = List.length !meow_cache in
            if len = 0 then
              SendMessage (chat.id, "Couldn't load any images", false, None, None)
            else
              SendMessage (chat.id, List.nth !meow_cache (Random.int len), false, None, None)
          end in
      let quote = function
        | {chat; reply_to_message = Some ({text = Some text} as msg)} ->
          Db.Quotes.put ~quote:text ~who:(get_sender msg) ();
          SendMessage (chat.id, "Quoting " ^ get_sender msg ^ " who said:\n" ^ text, false, None, None)
        | {chat; reply_to_message = Some msg} ->
          SendMessage (chat.id, "Quoting " ^ get_sender msg ^ " who said nothing", false, None, None)
        | {chat; text = Some text} ->
          match tokenize text with
          | keyword::_ ->
            SendMessage (chat.id, Db.Quotes.search ~keyword, false, None, None)
          | [] ->
            let (sender, msg, time) = Db.Quotes.get_random () in
            SendMessage (chat.id, sender ^ " said:\n" ^ msg, false, None, None) in
      let sed = function
        | {chat; message_id; text = Some text; reply_to_message = Some {text = Some original}} ->
          let open Batteries.String in
          let open Batteries.Tuple in
          let (_, cmd) = split text ~by:" " in
          let (sub, by) = Tuple2.mapn strip (split cmd ~by:"/") in
          let (_, result) = replace ~str:original ~sub ~by in
          SendMessage (chat.id, result, false, Some message_id, None)
        | {chat; message_id} -> SendMessage (chat.id, "Invalid usage of /sed", false, Some message_id, None) in
      let decide = function
        | {chat; message_id; text = Some text} ->
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
            SendMessage (chat.id, "Give me options nerd", false, Some message_id, None)
          else if len = 1 then
            SendMessage (chat.id, List.nth ["yes"; "no"] (Random.int 2), false, Some message_id, None)
          else
            SendMessage (chat.id, List.nth options (Random.int len), false, Some message_id, None)
        | _ -> Nothing in
      let jukebox = function
        | {chat; text = Some text; reply_to_message = Some {audio = Some {file_id}}} -> begin
            let open Batteries.String in
            let (_, info) = split text ~by:" " in
            let (performer, title) = split info ~by:"-" in
            Db.Jukebox.put ~title:(trim @@ title) ~performer ~file_id ();
            SendMessage (chat.id, "Added " ^ info ^ " to the database!", false, None, None)
          end
        | {chat; message_id; text = Some text} -> begin
            let open Batteries.String in
            match tokenize text with
            | [] -> SendMessage (chat.id, concat "\n" (Db.Jukebox.list ()), false, None, None)
            | xs -> begin
                let song = trim @@ concat " " xs in
                let (title, performer, file_id) = Db.Jukebox.search ~title:song in
                ResendAudio (chat.id, file_id, performer, title, false, Some message_id, None)
              end
          end
        | {chat; message_id} -> SendMessage (chat.id, "Invalid input", false, Some message_id, None) in
      let share_audio song performer title = function
        | {chat; message_id} -> ResendAudio (chat.id, song, performer, title, false, Some message_id, None) in
      let unfree = function
        | {chat} -> SendVoice (chat.id, "data/free.ogg", false, None, None, function Success id -> Nothing
                                                                                   | Failure er -> SendMessage (chat.id, "Failed to send audio with: " ^ er, false, None, None)) in
      let dab = function
        | {chat} -> Chain (SendPhoto (chat.id, "data/dab.jpg", Some "Bitch, dab", false, None, None, function Success id -> Nothing
                                                                                                            | Failure er -> SendMessage (chat.id, "Failed to send photo with: " ^ er, false, None, None)),
                           ResendAudio (chat.id, "BQADAQADcwADi_LrCbRvyK66JIVTAg", "Migos", "Pipe It Up", false, None, None)) in
      [{name = "hello"; description = "Greet the user"; enabled = true; run = greet};
       {name = "meow"; description = "Load images from /r/meow_irl"; enabled = true; run = meow};
       {name = "free"; description = "Free the world from the clutches of proprietary software"; enabled = true; run = share_audio "BQADAQADcQADi_LrCWoG5Wp27N76Ag" "Richard M. Stallman" "Free Software Song"};
       {name = "ocaml"; description = "Shill OCaml"; enabled = true; run = share_audio "BQADAQADcgADi_LrCdarRiXyyEZbAg" "Nate Foster" "flOCaml"};
       {name = "dab"; description = "Pipe it up"; enabled = true; run = dab};
       {name = "unfree"; description = "Testing voice API"; enabled = true; run = unfree};
       {name = "q"; description = "Save a quote"; enabled = true; run = quote};
       {name = "sed"; description = "Correct text"; enabled = true; run = sed};
       {name = "jukebox"; description = "Store and play music"; enabled = true; run = jukebox};
       {name = "decide"; description = "Help make a decision"; enabled = true; run = decide}]

    let new_chat_member chat user =
      let open Api.Chat in
      let title = match chat.title with Some x -> x | _ -> "this shithole" in
      SendMessage (chat.id, "Welcome to " ^ title ^ " " ^ Api.User.(user.first_name), false, None, None)

    let left_chat_member chat user =
      let open Api.Chat in
      SendMessage (chat.id, "lmao look at this nerd dude just got roasted", false, None, None)
end)

let () = Glg.run ()
