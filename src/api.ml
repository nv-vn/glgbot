open Yojson.Safe

exception ApiException of string

let rec get_field target = function
  | `Assoc [] -> raise (ApiException "Could not read JSON field!")
  | `Assoc (x::xs) when fst x = target -> snd x
  | `Assoc (x::xs) -> get_field target (`Assoc xs)
  | _ -> raise (ApiException "Invalid field access!")

let rec get_opt_field target = function
  | `Assoc [] -> None
  | `Assoc (x::xs) when fst x = target -> Some (snd x)
  | `Assoc (x::xs) -> get_opt_field target (`Assoc xs)
  | _ -> raise (ApiException "Invalid field access!")

let (>>=) x f = match x with
  | Some x -> f x
  | None -> None

let (<$>) f x = match x with
  | Some x -> Some (f x)
  | None -> None

let the_string = function
  | `String string -> string
  | _ -> raise (ApiException "Type assertion failed!")

let this_string x = `String x

let the_int = function
  | `Int int -> int
  | _ -> raise (ApiException "Type assertion failed!")

let this_int x = `Int x

let the_float = function
  | `Float float -> float
  | _ -> raise (ApiException "Type assertion failed!")

let this_float x = `Float x

let the_list = function
  | `List list -> list
  | _ -> raise (ApiException "Type assertion failed!")

let this_list xs = `List xs

let (+?) xs = function
  | (_, None) -> xs
  | (name, Some y) -> xs @ [name, y]

module User = struct
  type user = {
    id         : int;
    first_name : string;
    last_name  : string option;
    username   : string option
  }

  let create ~id ~first_name ?(last_name=None) ?(username=None) () =
    {id; first_name; last_name; username}

  let read obj =
    let id = the_int @@ get_field "id" obj in
    let first_name = the_string @@ get_field "first_name" obj in
    let last_name = the_string <$> get_opt_field "last_name" obj in
    let username = the_string <$> get_opt_field "last_name" obj in
    create ~id ~first_name ~last_name ~username ()
end

module Chat = struct
  type chat_type = Private | Group | Supergroup | Channel

  let read_type = function
    | "private" -> Private
    | "group" -> Group
    | "supergroup" -> Supergroup
    | "channel" -> Channel
    | _ -> raise (ApiException "Unknown chat type!")

  type chat = {
    id         : int;
    chat_type  : chat_type;
    title      : string option;
    username   : string option;
    first_name : string option;
    last_name  : string option
  }

  let create ~id ~chat_type ?(title=None) ?(username=None) ?(first_name=None) ?(last_name=None) () =
    {id; chat_type; title; username; first_name; last_name}

  let read obj =
    let id = the_int @@ get_field "id" obj in
    let chat_type = read_type @@ the_string @@ get_field "type" obj in
    let title = the_string <$> get_opt_field "title" obj in
    let username = the_string <$> get_opt_field "username" obj in
    let first_name = the_string <$> get_opt_field "first_name" obj in
    let last_name = the_string <$> get_opt_field "last_name" obj in
    create ~id ~chat_type ~title ~username ~first_name ~last_name ()
end

module InputFile = struct
  open Lwt

  let load (file:string) =
    let open Lwt_io in
    with_file ~mode:input file read

  let multipart_body fields (name, file, mime) boundary' =
    let boundary = "--" ^ boundary' in
    let ending = boundary ^ "--"
    and break = "\r\n" in
    load file >>= fun file_bytes ->
    let field_bodies = List.map (fun (name, value) ->
        boundary ^ break
        ^ "Content-Disposition: form-data; name=\"" ^ name ^ "\"" ^ break ^ break
        ^ value ^ break) fields |> fun strs -> List.fold_right (^) strs "" in
    let file_body =
      boundary ^ break
      ^ "Content-Disposition: form-data; name=\"" ^ name ^ "\"; filename=\"" ^ file ^ "\"" ^ break
      ^ "Content-Type: " ^ mime ^ break ^ break
      ^ file_bytes ^ break in
    return @@ field_bodies ^ file_body ^ ending
end

module PhotoSize = struct
  type photo_size = {
    file_id   : string;
    width     : int;
    height    : int;
    file_size : int option
  }

  let create ~file_id ~width ~height ?(file_size = None) () =
    {file_id; width; height; file_size}

  let read obj =
    let file_id = the_string @@ get_field "file_id" obj in
    let width = the_int @@ get_field "width" obj in
    let height = the_int @@ get_field "height" obj in
    let file_size = the_int <$> get_opt_field "file_size" obj in
    create ~file_id ~width ~height ~file_size ()

  module Out = struct
    type photo_size = {
      chat_id             : int;
      photo               : string;
      caption             : string option;
      reply_to_message_id : int option;
      reply_markup        : unit option (* FIXME *)
    }

    let create ~chat_id ~photo ?(caption = None) ?(reply_to = None) () =
      {chat_id; photo; caption; reply_to_message_id = reply_to; reply_markup = None}

    let prepare = function
    | {chat_id; photo; caption; reply_to_message_id; reply_markup} ->
      let json = `Assoc ([("chat_id", `Int chat_id);
                          ("photo", `String photo)] +? ("caption", this_string <$> caption)
                                                    +? ("reply_to_message_id", this_int <$> reply_to_message_id)) in
      Yojson.Safe.to_string json

    let prepare_multipart = function
      | {chat_id; photo; caption; reply_to_message_id; reply_markup} ->
        let fields = ([("chat_id", string_of_int chat_id)] +? ("caption", caption)
                                                           +? ("reply_to_message_id", string_of_int <$> reply_to_message_id)) in
        let open Batteries.String in
        let mime =
          if ends_with photo ".jpg" || ends_with photo ".jpeg" then "image/jpeg" else
          if ends_with photo ".png" then "image/png" else "text/plain" in
        InputFile.multipart_body fields ("photo", photo, mime)
  end
end

module Audio = struct
  type audio = {
    file_id   : string;
    duration  : int;
    performer : string option;
    title     : string option;
    mime_type : string option;
    file_size : int option
  }

  let create ~file_id ~duration ?(performer = None) ?(title = None) ?(mime_type = None) ?(file_size = None) () =
    {file_id; duration; performer; title; mime_type; file_size}

  let read obj =
    let file_id = the_string @@ get_field "file_id" obj in
    let duration = the_int @@ get_field "duration" obj in
    let performer = the_string <$> get_opt_field "performer" obj in
    let title = the_string <$> get_opt_field "title" obj in
    let mime_type = the_string <$> get_opt_field "mime_type" obj in
    let file_size = the_int <$> get_opt_field "file_size" obj in
    create ~file_id ~duration ~performer ~title ~mime_type ~file_size ()

  module Out = struct
    type audio = {
      chat_id             : int;
      audio               : string;
      duration            : int option;
      performer           : string;
      title               : string;
      reply_to_message_id : int option;
      reply_markup        : unit option (* FIXME *)
    }

    let create ~chat_id ~audio ?(duration = None) ~performer ~title ?(reply_to = None) () =
      {chat_id; audio; duration; performer; title; reply_to_message_id = reply_to; reply_markup = None}

    let prepare = function
    | {chat_id; audio; duration; performer; title; reply_to_message_id; reply_markup} ->
      let json = `Assoc ([("chat_id", `Int chat_id);
                          ("audio", `String audio);
                          ("performer", `String performer);
                          ("title", `String title)] +? ("duration", this_int <$> duration)
                                                    +? ("reply_to_message_id", this_int <$> reply_to_message_id)) in
      Yojson.Safe.to_string json

    let prepare_multipart = function
      | {chat_id; audio; duration; performer; title; reply_to_message_id} ->
        let fields = [("chat_id", string_of_int chat_id);
                      ("performer", performer);
                      ("title", title)] +? ("duration", string_of_int <$> duration)
                                        +? ("reply_to_message_id", string_of_int <$> reply_to_message_id) in
        InputFile.multipart_body fields ("audio", audio, "audio/mpeg")
  end
end

module Document = struct
  type document = {
    file_id   : string;
    thumb     : PhotoSize.photo_size option;
    file_name : string option;
    mime_type : string option;
    file_size : int option
  }

  let create ~file_id ?(thumb = None) ?(file_name = None) ?(mime_type = None) ?(file_size = None) () =
    {file_id; thumb; file_name; mime_type; file_size}

  let read obj =
    let file_id = the_string @@ get_field "file_id" obj in
    let thumb = PhotoSize.read <$> get_opt_field "thumb" obj in
    let file_name = the_string <$> get_opt_field "file_name" obj in
    let mime_type = the_string <$> get_opt_field "mime_type" obj in
    let file_size = the_int <$> get_opt_field "file_size" obj in
    create ~file_id ~thumb ~file_name ~mime_type ~file_size ()
end

module Sticker = struct
  type sticker = {
    file_id   : string;
    width     : int;
    height    : int;
    thumb     : PhotoSize.photo_size option;
    file_size : int option
  }

  let create ~file_id ~width ~height ?(thumb = None) ?(file_size = None) () =
    {file_id; width; height; thumb; file_size}

  let read obj =
    let file_id = the_string @@ get_field "file_id" obj in
    let width = the_int @@ get_field "width" obj in
    let height = the_int @@ get_field "height" obj in
    let thumb = PhotoSize.read <$> get_opt_field "thumb" obj in
    let file_size = the_int <$> get_opt_field "file_size" obj in
    create ~file_id ~width ~height ~thumb ~file_size ()
end

module Video = struct
  type video = {
    file_id   : string;
    width     : int;
    height    : int;
    duration  : int;
    thumb     : PhotoSize.photo_size option;
    mime_type : string option;
    file_size : int option
  }

  let create ~file_id ~width ~height ~duration ?(thumb = None) ?(mime_type = None) ?(file_size = None) () =
    {file_id; width; height; duration; thumb; mime_type; file_size}

  let read obj =
    let file_id = the_string @@ get_field "file_id" obj in
    let width = the_int @@ get_field "width" obj in
    let height = the_int @@ get_field "height" obj in
    let duration = the_int @@ get_field "duration" obj in
    let thumb = PhotoSize.read <$> get_opt_field "thumb" obj in
    let mime_type = the_string <$> get_opt_field "mime_type" obj in
    let file_size = the_int <$> get_opt_field "file_size" obj in
    create ~file_id ~width ~height ~duration ~thumb ~mime_type ~file_size ()
end

module Voice = struct
  type voice = {
    file_id   : string;
    duration  : int;
    mime_type : string option;
    file_size : int option
  }

  let create ~file_id ~duration ?(mime_type = None) ?(file_size = None) () =
    {file_id; duration; mime_type; file_size}

  let read obj =
    let file_id = the_string @@ get_field "file_id" obj in
    let duration = the_int @@ get_field "duration" obj in
    let mime_type = the_string <$> get_opt_field "mime_type" obj in
    let file_size = the_int <$> get_opt_field "file_size" obj in
    create ~file_id ~duration ~mime_type ~file_size ()

  module Out = struct
    type voice = {
      chat_id             : int;
      voice               : string;
      duration            : int option;
      reply_to_message_id : int option;
      reply_markup        : unit option (* FIXME *)
    }

    let create ~chat_id ~voice ?(duration = None) ?(reply_to = None) () =
      {chat_id; voice; duration; reply_to_message_id = reply_to; reply_markup = None}

    let prepare = function
      | {chat_id; voice; duration; reply_to_message_id} ->
        let json = `Assoc ([("chat_id", `Int chat_id);
                            ("voice", `String voice)]  +? ("duration", this_int <$> duration)
                                                       +? ("reply_to_message_id", this_int <$> reply_to_message_id)) in
        Yojson.Safe.to_string json

    let prepare_multipart = function
      | {chat_id; voice; duration; reply_to_message_id} ->
        let fields = [("chat_id", string_of_int chat_id)] +? ("duration", string_of_int <$> duration)
                                                          +? ("reply_to_message_id", string_of_int <$> reply_to_message_id) in
        InputFile.multipart_body fields ("voice", voice, "audio/ogg")
  end
end

module Contact = struct
  type contact = {
    phone_number : string;
    first_name   : string;
    last_name    : string option;
    user_id      : int option
  }

  let create ~phone_number ~first_name ?(last_name = None) ?(user_id = None) () =
    {phone_number; first_name; last_name; user_id}

  let read obj =
    let phone_number = the_string @@ get_field "phone_number" obj in
    let first_name = the_string @@ get_field "first_name" obj in
    let last_name = the_string <$> get_opt_field "last_name" obj in
    let user_id = the_int <$> get_opt_field "user_id" obj in
    create ~phone_number ~first_name ~last_name ~user_id ()
end

module Location = struct
  type location = {
    longitude : float;
    latitude  : float
  }

  let create ~longitude ~latitude () =
    {longitude; latitude}

  let read obj =
    let longitude = the_float @@ get_field "longitude" obj in
    let latitude = the_float @@ get_field "latitude" obj in
    create ~longitude ~latitude ()
end

module Message = struct
  open Chat
  open User

  (* Lots more fields to support... *)
  type message = {
    message_id       : int;
    from             : User.user option;
    date             : int;
    chat             : Chat.chat;
    forward_from     : User.user option;
    forward_date     : int option;
    reply_to_message : message option;
    text             : string option;
    audio            : Audio.audio option;
    document         : Document.document option;
    photo            : PhotoSize.photo_size list option;
    sticker          : Sticker.sticker option;
    video            : Video.video option;
    voice            : Voice.voice option;
    caption          : string option;
    contact          : Contact.contact option;
    location         : Location.location option
  }

  let create ~message_id ?(from = None) ~date ~chat ?(forward_from = None) ?(forward_date = None) ?(reply_to = None) ?(text = None) ?(audio = None) ?(document = None) ?(photo = None) ?(sticker = None) ?(video = None) ?(voice = None) ?(caption = None) ?(contact = None) ?(location = None) () =
    {message_id; from; date; chat; forward_from; forward_date; reply_to_message = reply_to; text; audio; document; photo; sticker; video; voice; caption; contact; location}

  let rec read obj =
    let message_id = the_int @@ get_field "message_id" obj in
    let from = User.read <$> get_opt_field "from" obj in
    let date = the_int @@ get_field "date" obj in
    let chat = Chat.read @@ get_field "chat" obj in
    let forward_from = User.read <$> get_opt_field "forward_from" obj in
    let forward_date = the_int <$> get_opt_field "forward_date" obj in
    let reply_to = read <$> get_opt_field "reply_to_message" obj in
    let text = the_string <$> get_opt_field "text" obj in
    let audio = Audio.read <$> get_opt_field "audio" obj in
    let document = Document.read <$> get_opt_field "document" obj in
    let photo = List.map PhotoSize.read <$> (the_list <$> get_opt_field "photo" obj) in
    let sticker = Sticker.read <$> get_opt_field "sticker" obj in
    let video = Video.read <$> get_opt_field "video" obj in
    let voice = Voice.read <$> get_opt_field "voice" obj in
    let caption = the_string <$> get_opt_field "caption" obj in
    let contact = Contact.read <$> get_opt_field "contact" obj in
    let location = Location.read <$> get_opt_field "location" obj in
    create ~message_id ~from ~date ~chat ~forward_from ~forward_date ~reply_to ~text ~audio ~document ~photo ~sticker ~video ~voice ~caption ~contact ~location ()

  let get_sender_first_name = function
    | {from = Some user} -> user.first_name
    | {chat = {first_name = Some first_name}} -> first_name
    | _ -> "unknown sender"

  let get_sender_username = function
    | {from = Some {username = Some username}} -> username
    | {chat = {username = Some username}} -> username
    | _ -> ""

  let get_sender msg =
    match get_sender_username msg with
    | "" -> get_sender_first_name msg
    | un -> get_sender_first_name msg ^ " (" ^ un ^ ")"
end

module Update = struct
  type update = {
    update_id : int;
    message   : Message.message option
  }

  let create ~update_id ?(message=None) () =
    {update_id; message}

  let read obj =
    let update_id = the_int @@ get_field "update_id" obj in
    let message = Message.read <$> get_opt_field "message" obj in
    create ~update_id ~message ()
end

module Result = struct
  type 'a result = Success of 'a | Failure of string

  let return x = Success x

  let default x = function
    | Success x -> x
    | Failure _ -> x

  let (>>=) x f = match x with
    | Success x -> f x
    | Failure err -> Failure err

  let (<$>) f = function
    | Success x -> Success (f x)
    | Failure err -> Failure err
end

module Command = struct
  open Update
  open Message
  open Batteries.String

  type action =
    | Nothing
    | GetMe of (User.user Result.result -> action)
    | SendMessage of int * string
    | SendPhoto of int * string * string option * int option * (string Result.result -> action)
    | ResendPhoto of int * string * string option * int option
    | SendAudio of int * string * string * string * int option * (string Result.result -> action)
    | ResendAudio of int * string * string * string * int option
    | SendVoice of int * string * int option * (string Result.result -> action)
    | ResendVoice of int * string * int option
    | GetUpdates of (Update.update list Result.result -> action)
    | PeekUpdate of (Update.update Result.result -> action)
    | PopUpdate of (Update.update Result.result -> action)
    | Chain of action * action

  type command = {
    name            : string;
    description     : string;
    mutable enabled : bool;
    run             : message -> action
  }

  let is_command = function
    | {message = Some {text = Some txt}} when starts_with txt "/" -> true
    | _ -> false

  let rec read_command msg cmds = match msg with
    | {text = Some txt; _} -> begin
        let cmp str cmd =
          match nsplit str ~by:" " with
          | [] -> false
          | a::_ -> begin
              match nsplit a ~by:"@" with
              | [] -> false
              | a::_ -> a = cmd
            end in
        match cmds with
        | [] -> Nothing
        | cmd::_ when cmp txt ("/" ^ cmd.name) && cmd.enabled -> cmd.run msg
        | _::cmds -> read_command msg cmds
      end
    | {text = None} -> Nothing

  let read_update = function
    | {message = Some msg} -> read_command msg
    | _ -> fun _ -> Nothing

  let tokenize msg = List.tl @@ nsplit msg ~by:" "

  let make_helper = function
    | {name; description} -> "/" ^ name ^ " - " ^ description

  let rec make_help = function
    | [] -> ""
    | cmd::cmds -> "\n" ^ make_helper cmd ^ make_help cmds
end

module type BOT = sig
  val token : string
  val commands : Command.command list
end

module type TELEGRAM_BOT = sig
  val url : string
  val commands : Command.command list

  val get_me : User.user Result.result Lwt.t
  val send_message : chat_id:int -> text:string -> unit Result.result Lwt.t
  val send_photo : chat_id:int -> photo:string -> ?caption:string option -> reply_to:int option -> string Result.result Lwt.t
  val resend_photo : chat_id:int -> photo:string -> ?caption:string option -> reply_to:int option -> unit Result.result Lwt.t
  val send_audio : chat_id:int -> audio:string -> performer:string -> title:string -> reply_to:int option -> string Result.result Lwt.t
  val resend_audio : chat_id:int -> audio:string -> performer:string -> title:string -> reply_to:int option -> unit Result.result Lwt.t
  val send_voice : chat_id:int -> voice:string -> reply_to:int option -> string Result.result Lwt.t
  val resend_voice : chat_id:int -> voice:string -> reply_to:int option -> unit Result.result Lwt.t
  val get_updates : Update.update list Result.result Lwt.t
  val peek_update : Update.update Result.result Lwt.t
  val pop_update : ?run_cmds:bool -> unit -> Update.update Result.result Lwt.t
end

module Mk (B : BOT) = struct
  open Lwt
  open Cohttp
  open Cohttp_lwt_unix

  open Command

  let url = "https://api.telegram.org/bot" ^ B.token ^ "/"
  let rec commands =
    let open Chat in
    let open Message in
    {name = "help"; description = "Show this message"; enabled = true; run = function
         | {chat} -> SendMessage (chat.id, "Commands:" ^ Command.make_help commands)} :: B.commands

  let get_me =
    Client.get (Uri.of_string (url ^ "getMe")) >>= fun (resp, body) ->
    Cohttp_lwt_body.to_string body >>= fun json ->
    let obj = Yojson.Safe.from_string json in
    return @@ match get_field "ok" obj with
    | `Bool true -> Result.Success (User.read @@ get_field "result" obj)
    | _ -> Result.Failure (the_string @@ get_field "description" obj)

  let send_message ~chat_id ~text =
    let json = `Assoc [("chat_id", `Int chat_id);
                       ("text", `String text)] in
    let body = Yojson.Safe.to_string json in
    let headers = Cohttp.Header.init_with "Content-Type" "application/json" in
    Client.post ~headers ~body:(Cohttp_lwt_body.of_string body) (Uri.of_string (url ^ "sendMessage")) >>= fun (resp, body) ->
    Cohttp_lwt_body.to_string body >>= fun json ->
    let obj = Yojson.Safe.from_string json in
    return @@ match get_field "ok" obj with
    | `Bool true -> Result.Success ()
    | _ -> Result.Failure (the_string @@ get_field "description" obj)

  let send_photo ~chat_id ~photo ?(caption = None) ~reply_to =
    let boundary = "--1234567890" in
    PhotoSize.Out.prepare_multipart (PhotoSize.Out.create ~chat_id ~photo ~caption ~reply_to ()) boundary >>= fun body ->
    let headers = Cohttp.Header.init_with "Content-Type" ("multipart/form-data; boundary=" ^ boundary) in
    Client.post ~headers ~body:(Cohttp_lwt_body.of_string body) (Uri.of_string (url ^ "sendPhoto")) >>= fun (resp, body) ->
    Cohttp_lwt_body.to_string body >>= fun json ->
    let obj = Yojson.Safe.from_string json in
    return @@ match get_field "ok" obj with
    | `Bool true -> Result.Success (the_string @@ get_field "file_id" @@ List.hd @@ the_list @@ get_field "photo" @@ get_field "result" obj)
    | _ -> Result.Failure ((fun x -> print_endline x; x) @@ the_string @@ get_field "description" obj)

  let resend_photo ~chat_id ~photo ?(caption = None) ~reply_to =
    let body = PhotoSize.Out.prepare @@ PhotoSize.Out.create ~chat_id ~photo ~caption ~reply_to () in
    let headers = Cohttp.Header.init_with "Content-Type" "application/json" in
    Client.post ~headers ~body:(Cohttp_lwt_body.of_string body) (Uri.of_string (url ^ "sendPhoto")) >>= fun (resp, body) ->
    Cohttp_lwt_body.to_string body >>= fun json ->
    let obj = Yojson.Safe.from_string json in
    return @@ match get_field "ok" obj with
    | `Bool true -> Result.Success ()
    | _ -> Result.Failure ((fun x -> print_endline x; x) @@ the_string @@ get_field "description" obj)

  let send_audio ~chat_id ~audio ~performer ~title ~reply_to =
    let boundary = "---1234567890" in
    Audio.Out.prepare_multipart (Audio.Out.create ~chat_id ~audio ~performer ~title ~reply_to ()) boundary >>= fun body ->
    let headers = Cohttp.Header.init_with "Content-Type" ("multipart/form-data; boundary=" ^ boundary) in
    Client.post ~headers ~body:(Cohttp_lwt_body.of_string body) (Uri.of_string (url ^ "sendAudio")) >>= fun (resp, body) ->
    Cohttp_lwt_body.to_string body >>= fun json ->
    let obj = Yojson.Safe.from_string json in
    return @@ match get_field "ok" obj with
    | `Bool true -> Result.Success (the_string @@ get_field "file_id" @@ get_field "audio" @@ get_field "result" obj)
    | _ -> Result.Failure ((fun x -> print_endline x; x) @@ the_string @@ get_field "description" obj)

  let resend_audio ~chat_id ~audio ~performer ~title ~reply_to =
    let body = Audio.Out.prepare @@ Audio.Out.create ~chat_id ~audio ~performer ~title ~reply_to () in
    let headers = Cohttp.Header.init_with "Content-Type" "application/json" in
    Client.post ~headers ~body:(Cohttp_lwt_body.of_string body) (Uri.of_string (url ^ "sendAudio")) >>= fun (resp, body) ->
    Cohttp_lwt_body.to_string body >>= fun json ->
    let obj = Yojson.Safe.from_string json in
    return @@ match get_field "ok" obj with
    | `Bool true -> Result.Success ()
    | _ -> Result.Failure ((fun x -> print_endline x; x) @@ the_string @@ get_field "description" obj)

  let send_voice ~chat_id ~voice ~reply_to =
    let boundary = "---1234567890" in
    Voice.Out.prepare_multipart (Voice.Out.create ~chat_id ~voice ~reply_to ()) boundary >>= fun body ->
    let headers = Cohttp.Header.init_with "Content-Type" ("multipart/form-data; boundary=" ^ boundary) in
    Client.post ~headers ~body:(Cohttp_lwt_body.of_string body) (Uri.of_string (url ^ "sendVoice")) >>= fun (resp, body) ->
    Cohttp_lwt_body.to_string body >>= fun json ->
    let obj = Yojson.Safe.from_string json in
    return @@ match get_field "ok" obj with
    | `Bool true -> Result.Success (the_string @@ get_field "file_id" @@ get_field "voice" @@ get_field "result" obj)
    | _ -> Result.Failure ((fun x -> print_endline x; x) @@ the_string @@ get_field "description" obj)

  let resend_voice ~chat_id ~voice ~reply_to =
    let body = Voice.Out.prepare @@ Voice.Out.create ~chat_id ~voice ~reply_to () in
    let headers = Cohttp.Header.init_with "Content-Type" "application/json" in
    Client.post ~headers ~body:(Cohttp_lwt_body.of_string body) (Uri.of_string (url ^ "sendVoice")) >>= fun (resp, body) ->
    Cohttp_lwt_body.to_string body >>= fun json ->
    let obj = Yojson.Safe.from_string json in
    return @@ match get_field "ok" obj with
    | `Bool true -> Result.Success ()
    | _ -> Result.Failure ((fun x -> print_endline x; x) @@ the_string @@ get_field "description" obj)

  let get_updates =
    Client.get (Uri.of_string (url ^ "getUpdates")) >>= fun (resp, body) ->
    Cohttp_lwt_body.to_string body >>= fun json ->
    let obj = Yojson.Safe.from_string json in
    return @@ match get_field "ok" obj with
    | `Bool true -> Result.Success (List.map Update.read @@ the_list @@ get_field "result" obj)
    | _ -> Result.Failure (the_string @@ get_field "description" obj)

  let offset = ref 0
  let clear_update () =
    let json = `Assoc [("offset", `Int !offset);
                       ("limit", `Int 0)] in
    let body = Yojson.Safe.to_string json in
    let headers = Cohttp.Header.init_with "Content-Type" "application/json" in
    Client.post ~headers ~body:(Cohttp_lwt_body.of_string body) (Uri.of_string (url ^ "getUpdates")) >>= fun _ ->
    return ()

  (* TODO: Rename this & move elsewhere... *)
  let hd_ = function
    | [] -> Result.Failure "Could not get head"
    | x::_ -> Result.Success x

  let peek_update =
    let open Update in
    let json = `Assoc [("offset", `Int 0);
                       ("limit", `Int 1)] in
    let body = Yojson.Safe.to_string json in
    let headers = Cohttp.Header.init_with "Content-Type" "application/json" in
    Client.post ~headers ~body:(Cohttp_lwt_body.of_string body) (Uri.of_string (url ^ "getUpdates")) >>= fun (resp, body) ->
    Cohttp_lwt_body.to_string body >>= fun json ->
    let obj = Yojson.Safe.from_string json in
    let open Result in
    Lwt.return @@ match get_field "ok" obj with
    | `Bool true -> Update.read <$> (hd_ @@ the_list @@ get_field "result" obj)
    | _ -> Failure (the_string @@ get_field "description" obj)

  let rec pop_update ?(run_cmds=true) () =
    let open Update in
    let json = `Assoc [("offset", `Int !offset);
                       ("limit", `Int 1)] in
    let body = Yojson.Safe.to_string json in
    let headers = Cohttp.Header.init_with "Content-Type" "application/json" in
    Client.post ~headers ~body:(Cohttp_lwt_body.of_string body) (Uri.of_string (url ^ "getUpdates")) >>= fun (resp, body) ->
    Cohttp_lwt_body.to_string body >>= fun json ->
    let obj = Yojson.Safe.from_string json in
    match get_field "ok" obj with
    | `Bool true -> begin
        let open Result in
        let update = Update.read <$> (hd_ @@ the_list @@ get_field "result" obj) in
        offset := default !offset ((fun update -> update.update_id + 1) <$> update);
        let open Lwt in
        clear_update () >>= fun () ->
        if run_cmds && default false (Command.is_command <$> update) then begin
          ignore ((fun update -> evaluator @@ Command.read_update update commands) <$> update);
          return @@ ((fun update -> Update.create update.update_id ()) <$> update)
        end else return update
      end
    | _ -> return @@ Result.Failure (the_string @@ get_field "description" obj)

  and evaluator = function
    | Nothing -> return ()
    | GetMe f -> get_me >>= fun x -> evaluator (f x)
    | SendMessage (chat_id, text) -> send_message ~chat_id ~text >>= fun _ -> return ()
    | SendPhoto (chat_id, photo, caption, reply_to, f) -> send_photo ~chat_id ~photo ~caption ~reply_to >>= fun x -> evaluator (f x)
    | ResendPhoto (chat_id, photo, caption, reply_to) -> resend_photo ~chat_id ~photo ~caption ~reply_to >>= fun _ -> return ()
    | SendAudio (chat_id, audio, performer, title, reply_to, f) -> send_audio ~chat_id ~audio ~performer ~title ~reply_to >>= fun x -> evaluator (f x)
    | ResendAudio (chat_id, audio, performer, title, reply_to) -> resend_audio ~chat_id ~audio ~performer ~title ~reply_to >>= fun _ -> return ()
    | SendVoice (chat_id, voice, reply_to, f) -> send_voice ~chat_id ~voice ~reply_to >>= fun x -> evaluator (f x)
    | ResendVoice (chat_id, voice, reply_to) -> resend_voice ~chat_id ~voice ~reply_to >>= fun _ -> return ()
    | GetUpdates f -> get_updates >>= fun x -> evaluator (f x)
    | PeekUpdate f -> peek_update >>= fun x -> evaluator (f x)
    | PopUpdate f -> pop_update () >>= fun x -> evaluator (f x)
    | Chain (first, second) -> evaluator first >>= fun _ -> evaluator second
end
