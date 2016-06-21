open Lwt
open Yojson.Safe

open Telegram
open Telegram.Actions
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
        | {chat; message_id} as msg -> send_message ~chat_id:chat.id "Hello, %s" (get_sender msg) ~reply_to:message_id in
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
              send_message ~chat_id:chat.id "Couldn't load any images"
            else
              send_message ~chat_id:chat.id "%s" (List.nth !meow_cache (Random.int len))
          end in
      let quote = function
        | {chat; reply_to_message = Some ({text = Some text} as msg)} ->
          Db.Quotes.put ~quote:text ~who:(get_sender msg) ();
          send_message ~chat_id:chat.id "Quoting %s who said:\n%s" (get_sender msg) text
        | {chat; reply_to_message = Some msg} ->
          send_message ~chat_id:chat.id "Quoting %s who said nothing" (get_sender msg)
        | {chat} ->
          let (sender, msg, time) = Db.Quotes.get_random () in
          send_message ~chat_id:chat.id "%s said:\n%s" sender msg in
      let sed = function
        | {chat; message_id; text = Some text; reply_to_message = Some {text = Some original}} ->
          let open Batteries.String in
          let open Batteries.Tuple in
          let (_, cmd) = split text ~by:" " in
          let (sub, by) = Tuple2.mapn strip (split cmd ~by:"/") in
          let (_, result) = replace ~str:original ~sub ~by in
          send_message ~chat_id:chat.id "%s" result ~reply_to:message_id
        | {chat; message_id} -> send_message ~chat_id:chat.id "Invalid usage of /sed" ~reply_to:message_id in
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
            send_message ~chat_id:chat.id "Give me options nerd" ~reply_to:message_id
          else if len = 1 then
            send_message ~chat_id:chat.id "%s" (List.nth ["yes"; "no"] (Random.int 2)) ~reply_to:message_id
          else
            send_message ~chat_id:chat.id "%s" (List.nth options (Random.int len)) ~reply_to:message_id
        | _ -> nothing in
      let jukebox = function
        | {chat; text = Some text; reply_to_message = Some {audio = Some {file_id}}} -> begin
            let open Batteries.String in
            let (_, info) = split text ~by:" " in
            let (performer, title) = split info ~by:"-" in
            Db.Jukebox.put ~title:(trim @@ title) ~performer ~file_id ();
            send_message ~chat_id:chat.id "Added %s to the database!" info
          end
        | {chat; message_id; text = Some text} -> begin
            let open Batteries.String in
            match tokenize text with
            | [] -> send_message ~chat_id:chat.id "%s" (concat "\n" (Db.Jukebox.list ()))
            | xs -> begin
                let song = trim @@ concat " " xs in
                let (title, performer, file_id) = Db.Jukebox.search ~title:song in
                resend_audio ~chat_id:chat.id file_id ~performer ~title ~reply_to:message_id
              end
          end
        | {chat; message_id} -> send_message ~chat_id:chat.id "Invalid input" ~reply_to:message_id in
      let share_audio song performer title = function
        | {chat; message_id} -> resend_audio ~chat_id:chat.id song ~performer ~title ~reply_to:message_id in
      let unfree = function
        | {chat} -> send_voice ~chat_id:chat.id "data/free.ogg" /> function Success id -> nothing
                                                                          | Failure er -> send_message ~chat_id:chat.id "Failed to send audio with: %s" er in
      let dab = function
        | {chat} -> send_photo ~chat_id:chat.id "data/dab.jpg" ~caption:"Bitch, dab" /> function Success id -> nothing
                                                                                               | Failure er -> send_message ~chat_id:chat.id "Failed to send photo with: %s" er
                    /+ resend_audio ~chat_id:chat.id "BQADAQADcwADi_LrCbRvyK66JIVTAg" ~performer:"Migos" ~title:"Pipe It Up" in
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
      send_message ~chat_id:chat.id "Welcome to %s, %s" title Api.User.(user.first_name)

    let left_chat_member chat user =
      let open Api.Chat in
      send_message ~chat_id:chat.id "lmao look at this nerd dude just got roasted"
end)

let () = Glg.run ()
