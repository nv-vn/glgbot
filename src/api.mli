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

module Response : sig
  type result =
    | User of User.user
    | Updates of Update.update list
    | Err of string
  val read : json -> result
end
