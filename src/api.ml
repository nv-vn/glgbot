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

let the_int = function
  | `Int int -> int
  | _ -> raise (ApiException "Type assertion failed!")

let the_list = function
  | `List list -> list
  | _ -> raise (ApiException "Type assertion failed!")

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
    id : int;
    chat_type : chat_type;
    title : string option;
    username : string option;
    first_name : string option;
    last_name : string option
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

module Message = struct
  open Chat
  open User

  (* Lots more fields to support... *)
  type message = {
    message_id : int;
    from : User.user option;
    date : int;
    chat : Chat.chat;
    text : string option
  }

  let create ~message_id ?(from=None) ~date ~chat ?(text=None) () =
    {message_id; from; date; chat; text}

  let read obj =
    let message_id = the_int @@ get_field "message_id" obj in
    let from = User.read <$> get_opt_field "from" obj in
    let date = the_int @@ get_field "date" obj in
    let chat = Chat.read @@ get_field "chat" obj in
    let text = the_string <$> get_opt_field "text" obj in
    create ~message_id ~from ~date ~chat ~text ()

  let get_sender = function
    | {from = Some user} -> user.first_name
    | {chat = {first_name = Some first_name}} -> first_name
    | _ -> "unknown sender"
end

module Update = struct
  type update = {
    update_id : int;
    message : Message.message option
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
    | GetMe of (User.user Result.result -> unit Lwt.t)
    | SendMessage of int * string
    | GetUpdates of (Update.update list Result.result -> unit Lwt.t)
    | PeekUpdate of (Update.update Result.result -> unit Lwt.t)
    | PopUpdate of (Update.update Result.result -> unit Lwt.t)
    | Chain of action * action

  type command = {
    name : string;
    run  : message -> action
  }

  let is_command = function
    | {message = Some {text = Some txt}} when starts_with txt "/" -> true
    | _ -> false

  let rec read_command msg cmds = match msg with
    | {text = Some txt; _} -> begin
        match cmds with
        | [] -> Nothing
        | cmd::_ when starts_with txt ("/" ^ cmd.name) -> cmd.run msg
        | _::cmds -> read_command msg cmds
      end
    | {text = None} -> Nothing

  let read_update = function
    | {message = Some msg} -> read_command msg
    | _ -> fun _ -> Nothing

  let tokenize msg = List.tl @@ nsplit msg ~by:" "
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
  let commands = B.commands

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

  let peek_update =
    let open Update in
    let json = `Assoc [("offset", `Int 0);
                       ("limit", `Int 1)] in
    let body = Yojson.Safe.to_string json in
    let headers = Cohttp.Header.init_with "Content-Type" "application/json" in
    Client.post ~headers ~body:(Cohttp_lwt_body.of_string body) (Uri.of_string (url ^ "getUpdates")) >>= fun (resp, body) ->
    Cohttp_lwt_body.to_string body >>= fun json ->
    let obj = Yojson.Safe.from_string json in
    return @@ match get_field "ok" obj with
    | `Bool true -> Result.Success (Update.read @@ List.hd @@ the_list @@ get_field "result" obj)
    | _ -> Result.Failure (the_string @@ get_field "description" obj)

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
        let update = Update.read @@ List.hd @@ the_list @@ get_field "result" obj in
        offset := update.update_id + 1;
        clear_update () >>= fun () ->
        if run_cmds && Command.is_command update then begin
          ignore @@ evaluator @@ Command.read_update update commands;
          return @@ Result.Success (Update.create update.update_id ())
        end else return @@ Result.Success update
      end
    | _ -> return @@ Result.Failure (the_string @@ get_field "description" obj)

  and evaluator = function
    | Nothing -> return ()
    | GetMe f -> get_me >>= f
    | SendMessage (chat_id, text) -> send_message ~chat_id ~text >>= fun _ -> return ()
    | GetUpdates f -> get_updates >>= f
    | PeekUpdate f -> peek_update >>= f
    | PopUpdate f -> pop_update () >>= f
    | Chain (first, second) -> evaluator first >>= fun _ -> evaluator second
end
