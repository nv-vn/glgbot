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

module type BOT = sig
  val token : string
end

module Mk (B : BOT) = struct
  open Lwt
  open Cohttp
  open Cohttp_lwt_unix

  let url = "https://api.telegram.org/bot" ^ B.token ^ "/"

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
    let headers  = Cohttp.Header.init_with "Content-Type" "application/json" in
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
end
