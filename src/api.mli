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
    id         : int;
    chat_type  : chat_type;
    title      : string option;
    username   : string option;
    first_name : string option;
    last_name  : string option
  }
  val create : id:int -> chat_type:chat_type -> ?title:string option -> ?username:string option -> ?first_name:string option -> ?last_name:string option -> unit -> chat
  val read : json -> chat
end

module InputFile : sig
  val load : string -> string Lwt.t
  val multipart_body : (string * string) list -> string * string * string -> string -> string Lwt.t
end

module Audio : sig
  type audio = {
    file_id   : string;
    duration  : int;
    performer : string option;
    title     : string option;
    mime_type : string option;
    file_size : int option
  }
  val create : file_id:string -> duration:int -> ?performer:string option -> ?title:string option -> ?mime_type:string option -> ?file_size:int option -> unit -> audio
  val read : json -> audio

  module Out : sig
    type audio = {
      chat_id             : int;
      audio               : string;
      duration            : int option;
      performer           : string;
      title               : string;
      reply_to_message_id : int option;
      reply_markup        : unit option (* FIXME *)
    }

    val create : chat_id:int -> audio:string -> ?duration:int option -> performer:string -> title:string -> ?reply_to:int option -> unit -> audio
    val prepare : audio -> string
    val prepare_multipart : audio -> string -> string Lwt.t
 end
end

module Voice : sig
  type voice = {
    chat_id             : int;
    voice               : string;
    duration            : int option;
    reply_to_message_id : int option;
    reply_markup        : unit option (* FIXME *)
  }

  val create : chat_id:int -> voice:string -> ?duration:int option -> ?reply_to:int option -> unit -> voice
  val prepare : voice -> string
  val prepare_multipart : voice -> string -> string Lwt.t
end

module Message : sig
  type message = {
    message_id       : int;
    from             : User.user option;
    date             : int;
    chat             : Chat.chat;
    forward_from     : User.user option;
    forward_date     : int option;
    reply_to_message : message option;
    text             : string option;
    audio            : Audio.audio option
  }
  val create : message_id:int -> ?from:User.user option -> date:int -> chat:Chat.chat -> ?forward_from:User.user option -> ?forward_date:int option -> ?reply_to:message option -> ?text:string option -> ?audio:Audio.audio option -> unit -> message
  val read : json -> message

  val get_sender : message -> string
end

module Update : sig
  type update = {
    update_id : int;
    message   : Message.message option
  }
  val create : update_id:int -> ?message:Message.message option -> unit -> update
  val read : json -> update
end

(** Used for representing results of various actions where a success or failure can occur. Contains helper functions to implement a monadic and functorial interface. *)
module Result : sig
  (** Stores the return value if a function succeeded or a string if the function failed *)
  type 'a result = Success of 'a | Failure of string

  (** Raise a normal value into a result (Success) *)
  val return : 'a -> 'a result

  (** Take the value of the result, if it succeeded, or the other argument by default and return that *)
  val default : 'a -> 'a result -> 'a

  (** Monadic bind *)
  val (>>=) : 'a result -> ('a -> 'b result) -> 'b result

  (** Functorial map *)
  val (<$>) : ('a -> 'b) -> 'a result -> 'b result
end

module Command : sig
  (** The actions that can be used by the bot's commands *)
  type action =
    | Nothing
    | GetMe of (User.user Result.result -> action)
    | SendMessage of int * string
    | SendAudio of int * string * string * string * int option * (string Result.result -> action)
    | ResendAudio of int * string * string * string * int option
    | SendVoice of int * string * int option * (string Result.result -> action)
    | ResendVoice of int * string * int option
    | GetUpdates of (Update.update list Result.result -> action)
    | PeekUpdate of (Update.update Result.result -> action)
    | PopUpdate of (Update.update Result.result -> action)
    | Chain of action * action

  (** This type is used to represent available commands. The `name` field is the command's name (without a slash) and the `description` field is the description to be used in the help message. `run` is the function called when invoking the command. *)
  type command = {
    name        : string;
    description : string;
    run         : Message.message -> action
  }


  (** Tests to see whether an update from the update queue invoked a command *)
  val is_command : Update.update -> bool

  (** Takes a message, known to represent a command, and a list of possible commands. These values are used to find the matching command and return the actions that it should perform *)
  val read_command : Message.message -> command list -> action

  (** Reads a single update and, given a list of commands, matches it to a correct command that can be invoked *)
  val read_update : Update.update -> command list -> action

  (** Turns a string into the args list that a command may choose to work with *)
  val tokenize : string -> string list
end

(** BOT is strictly used for customization of a TELEGRAM_BOT module. Once your customizations have been applied, pass it into Api.Mk to create
    the usable TELEGRAM_BOT interface. *)
module type BOT = sig
  (** The API token to use for the bot. Warning: please use ppx_blob to load this in at compile-time and add the blob to your .gitignore *)
  val token : string

  (** The list of commands that the bot will be able to use *)
  val commands : Command.command list
end

(** TELEGRAM_BOT represents the interface to a running bot *)
module type TELEGRAM_BOT = sig
  (** The base url for all connections to the API *)
  val url : string

  (** A list of all commands supported by the bot *)
  val commands : Command.command list

  (** Get the user information for the bot; use to test connection to the Telegram server *)
  val get_me : User.user Result.result Lwt.t

  (** Send a text message to a specified chat *)
  val send_message : chat_id:int -> text:string -> unit Result.result Lwt.t

  (** Send a new audio file (mp3) to a specified chat. Note that `audio` refers to the file's name to send. *)
  val send_audio : chat_id:int -> audio:string -> performer:string -> title:string -> reply_to:int option -> string Result.result Lwt.t

  (** Send an existing audio file (mp3) to a specified chat. Note that `audio` refers to the file's id on the Telegram servers. *)
  val resend_audio : chat_id:int -> audio:string -> performer:string -> title:string -> reply_to:int option -> unit Result.result Lwt.t

  (** Send a new voice message (ogg) to a specified chat. Note that `voice` refers to the file's name to send. *)
  val send_voice : chat_id:int -> voice:string -> reply_to:int option -> string Result.result Lwt.t

  (** Send an existing voice message (ogg) to a specified chat. Note that `voice` refers to the file's id on the Telegram servers. *)
  val resend_voice : chat_id:int -> voice:string -> reply_to:int option -> unit Result.result Lwt.t

  (** Get a list of all available updates that the bot has received *)
  val get_updates : Update.update list Result.result Lwt.t

  (** Get the first available update from the update queue *)
  val peek_update : Update.update Result.result Lwt.t

  (** Get the first available update from the update queue and mark it as read (deletes the update) *)
  val pop_update : ?run_cmds:bool -> unit -> Update.update Result.result Lwt.t
end

(** Generate a bot's interface to allow for direct calls to functions *)
module Mk : functor (B : BOT) -> TELEGRAM_BOT
