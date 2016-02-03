exception ApiException of string

type json = [ `Assoc of (string * json) list
            | `Bool of bool
            | `Float of float
            | `Int of int
            | `Intlit of string
            | `List of json list
            | `Null
            | `String of string
            | `Tuple of json list
            | `Variant of string * json option ]

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

module Response = struct
  type result =
    | User of User.user
    | Updates of Update.update list
    | Err of string

  let read obj =
    match get_field "ok" obj with
    | `Bool true -> begin
        match get_field "result" obj with
        | `List updates -> Updates (List.map Update.read updates)
        | `Assoc user -> User (User.read (`Assoc user))
        | _ -> Err "Couldn't read response!"
      end
    | _ -> Err (the_string @@ get_field "description" obj)
end
