open Yojson.Safe

exception ApiException of string

module User : sig
  type user = {
    id         : int;
    first_name : string;
    last_name  : string option;
    username   : string option
  }
  val create : id:int -> first_name:string -> ?last_name:string option -> ?username:string option -> unit -> user
  val read : json -> user
end

module Chat : sig
  type chat_type = Private | Group | Supergroup | Channel
  val read_type : string -> chat_type

  type chat = {
    id : int;
    chat_type : chat_type;
    title : string option;
    username : string option;
    first_name : string option;
    last_name : string option
  }
  val create : id:int -> chat_type:chat_type -> ?title:string option -> ?username:string option -> ?first_name:string option -> ?last_name:string option -> unit -> chat
  val read : json -> chat
end

module Message : sig
  type message = {
    message_id : int;
    from : User.user option;
    date : int;
    chat : Chat.chat;
    text : string option
  }
  val create : message_id:int -> ?from:User.user option -> date:int -> chat:Chat.chat -> ?text:string option -> unit -> message
  val read : json -> message
end

module Update : sig
  type update = {
    update_id : int;
    message : Message.message option
  }
  val create : update_id:int -> ?message:Message.message option -> unit -> update
  val read : json -> update
end

module Result : sig
  type 'a result = Success of 'a | Failure of string

  val return : 'a -> 'a result
  val default : 'a -> 'a result -> 'a
  val (>>=) : 'a result -> ('a -> 'b result) -> 'b result
  val (<$>) : ('a -> 'b) -> 'a result -> 'b result
end

module Command : sig
  type command = {
    name : string;
    run  : string list -> unit
  }

  val read_command : string -> command list -> unit
end

module type BOT = sig
  val token : string
  val commands : Command.command list
end

module Mk : functor (B : BOT) -> sig
  val url : string
  val commands : Command.command list

  val get_me : User.user Result.result Lwt.t
  val send_message : chat_id:int -> text:string -> unit Result.result Lwt.t
  val get_updates : Update.update list Result.result Lwt.t
  val peek_update : Update.update Result.result Lwt.t
  val pop_update : unit -> Update.update Result.result Lwt.t
end
