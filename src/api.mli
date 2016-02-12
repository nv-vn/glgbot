open Yojson.Safe

(** Specifies the API used for creating Telegram bots, defined at https://core.telegram.org/bots/api *)

(** An exception thrown if some rules specified in the API are invalidated by incorrectly formatted data of some type *)
exception ApiException of string

module User : sig
  (** Represents a user profile *)
  type user = {
    id         : int;
    first_name : string;
    last_name  : string option;
    username   : string option
  }
  (** Create a `user` in a concise manner *)
  val create : id:int -> first_name:string -> ?last_name:string option -> ?username:string option -> unit -> user
  (** Read a `user` out of some JSON *)
  val read : json -> user
end

(** Used to represent private messages, groupchats, and other types of Telegram chats *)
module Chat : sig
  (** The type of groupchat that the bot is in *)
  type chat_type = Private | Group | Supergroup | Channel
  (** Turn a string into a `chat_type` *)
  val read_type : string -> chat_type

  (** Represents a chat where messages can be sent or received *)
  type chat = {
    id         : int;
    chat_type  : chat_type;
    title      : string option;
    username   : string option;
    first_name : string option;
    last_name  : string option
  }
  (** Create a `chat` in a concise manner *)
  val create : id:int -> chat_type:chat_type -> ?title:string option -> ?username:string option -> ?first_name:string option -> ?last_name:string option -> unit -> chat
  (** Read a `chat` out of some JSON *)
  val read : json -> chat
end

(** Used for handling, loading, and sending outgoing files in messages *)
module InputFile : sig
  (** Loads a file (by filename) and returns the raw bytes inside of it *)
  val load : string -> string Lwt.t
(** Used to format data as HTTP `multipart/form-data`
    Takes:
    - A list of fields to be included in the form data as a pair of strings (name, value)
    - A tuple of: {ul
      {li The name of the data field}
      {li The path to the file/the file's name}
      {li The mime type of the file}}
    - A string to be used as a boundary to split different parts of the data; ideally, this text should 
not be present in the raw data of the file being sent
    @return The formatted string to use as the HTTP body (make sure to correctly format the headers for multipart/form-data) *)
  val multipart_body : (string * string) list -> string * string * string -> string -> string Lwt.t
end

(** This module is used for all images sent in chats *)
module PhotoSize : sig
  (** Represents any kind of image sent in a message or used as a thumbnail, profile picture, etc. *)
  type photo_size = {
    file_id   : string;
    width     : int;
    height    : int;
    file_size : int option
  }
  (** Create a `photo_size` in a concise manner *)
  val create : file_id:string -> width:int -> height:int -> ?file_size:int option -> unit -> photo_size
  (** Read a `photo_size` out of some JSON *)
  val read : json -> photo_size

  (** This module is used to deal with outgoing photo messages *)
  module Out : sig
    (** Represents the outgoing photo message. Note that the `photo` field can either be an existing file id or the raw bytes from a file *)
    type photo_size = {
      chat_id             : int;
      photo               : string;
      caption             : string option;
      reply_to_message_id : int option;
      reply_markup        : unit option (* FIXME *)
    }
    (** Create a `photo_size` in a concise manner *)
    val create : chat_id:int -> photo:string -> ?caption:string option -> ?reply_to:int option -> unit -> photo_size
    (** Prepare a `photo_size` for sending -- used in the case of a file id *)
    val prepare : photo_size -> string
    (** Prepare a `photo_size` for sending -- used in the case of the raw bytes *)
    val prepare_multipart : photo_size -> string -> string Lwt.t
  end
end

module Audio : sig
  (** Represents an audio message (mp3) *)
  type audio = {
    file_id   : string;
    duration  : int;
    performer : string option;
    title     : string option;
    mime_type : string option;
    file_size : int option
  }
  (** Create an `audio` in a concise manner *)
  val create : file_id:string -> duration:int -> ?performer:string option -> ?title:string option -> ?mime_type:string option -> ?file_size:int option -> unit -> audio
  (** Read an `audio` out of some JSON *)
  val read : json -> audio

  (** This module is used to deal with outgoing audio messages *)
  module Out : sig
    (** Represents the outgoing audio message. Note that the `audio` field can either be an existing file id or the raw bytes from a file *)
    type audio = {
      chat_id             : int;
      audio               : string;
      duration            : int option;
      performer           : string;
      title               : string;
      reply_to_message_id : int option;
      reply_markup        : unit option (* FIXME *)
    }
    (** Create an `audio` in a concise manner *)
    val create : chat_id:int -> audio:string -> ?duration:int option -> performer:string -> title:string -> ?reply_to:int option -> unit -> audio
    (** Prepare an `audio` for sending -- used in the case of a file id *)
    val prepare : audio -> string
    (** Prepare an `audio` for sending -- used in the case of the raw bytes *)
    val prepare_multipart : audio -> string -> string Lwt.t
 end
end

module Document : sig
  (** Represents a general file sent in a message *)
  type document = {
    file_id   : string;
    thumb     : PhotoSize.photo_size option;
    file_name : string option;
    mime_type : string option;
    file_size : int option
  }
  (** Create a `document` in a concise manner *)
  val create : file_id:string -> ?thumb:PhotoSize.photo_size option -> ?file_name:string option -> ?mime_type:string option -> ?file_size:int option -> unit -> document
  (** Read a `document` out of some JSON *)
  val read : json -> document
end

module Sticker : sig
  (** Represents sticker messages *)
  type sticker = {
    file_id   : string;
    width     : int;
    height    : int;
    thumb     : PhotoSize.photo_size option;
    file_size : int option
  }
  (** Create a `sticker` in a concise manner *)
  val create : file_id:string -> width:int -> height:int -> ?thumb:PhotoSize.photo_size option -> ?file_size:int option -> unit -> sticker
  (** Read a `sticker` out of some JSON *)
  val read : json -> sticker
end

module Video : sig
  (** Represents a video file sent in a message *)
  type video = {
    file_id   : string;
    width     : int;
    height    : int;
    duration  : int;
    thumb     : PhotoSize.photo_size option;
    mime_type : string option;
    file_size : int option
  }
  (** Create a `video` in a concise manner *)
  val create : file_id:string -> width:int -> height:int -> duration:int -> ?thumb:PhotoSize.photo_size option -> ?mime_type:string option -> ?file_size:int option -> unit -> video
  (** Read a `video` out of some JSON *)
  val read : json -> video
end

module Voice : sig
  (** Represents a voice message (ogg) *)
  type voice = {
    file_id   : string;
    duration  : int;
    mime_type : string option;
    file_size : int option
  }
  (** Create a `voice` in a concise manner *)
  val create : file_id:string -> duration:int -> ?mime_type:string option -> ?file_size:int option -> unit -> voice
  (** Read a `voice` out of some JSON *)
  val read : json -> voice

  (** This module is used to deal with outgoing voice messages *)
  module Out : sig
    (** Represents the outgoing voice message. Note that the `voice` field can either be an existing file id or the raw bytes from a file *)
    type voice = {
      chat_id             : int;
      voice               : string;
      duration            : int option;
      reply_to_message_id : int option;
      reply_markup        : unit option (* FIXME *)
    }
    (** Create a `voice` in a concise manner *)
    val create : chat_id:int -> voice:string -> ?duration:int option -> ?reply_to:int option -> unit -> voice
    (** Prepare a `voice` for sending -- used in the case of a file id *)
    val prepare : voice -> string
    (** Prepare a `voice` for sending -- used in the case of the raw bytes *)
    val prepare_multipart : voice -> string -> string Lwt.t
  end
end

module Contact : sig
  (** Represents a contact shared in a message *)
  type contact = {
    phone_number : string;
    first_name   : string;
    last_name    : string option;
    user_id      : int option
  }
  (** Create a `contact` in a concise manner *)
  val create : phone_number:string -> first_name:string -> ?last_name:string option -> ?user_id:int option -> unit -> contact
  (** Read a `contact` out of some JSON *)
  val read : json -> contact
end

module Location : sig
  (** Represents a location sent by a user in terms of longitude/latitude coordinates *)
  type location = {
    longitude : float;
    latitude  : float
  }
  (** Create a `location` in a concise manner *)
  val create : longitude:float -> latitude:float -> unit -> location
  (** Read a `location` out of some JSON *)
  val read : json -> location
end

module Message : sig
  (** Represents a message in a chat. Note that `photo` should be a list of `PhotoSize.photo_size`s if it exists *)
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
  (** Create a `message` in a concise manner *)
  val create : message_id:int -> ?from:User.user option -> date:int -> chat:Chat.chat -> ?forward_from:User.user option -> ?forward_date:int option -> ?reply_to:message option -> ?text:string option -> ?audio:Audio.audio option -> ?document:Document.document option -> ?photo:PhotoSize.photo_size list option -> ?sticker:Sticker.sticker option -> ?video:Video.video option -> ?voice:Voice.voice option -> ?caption:string option -> ?contact:Contact.contact option -> ?location:Location.location option -> unit -> message
  (** Read a `message` out of some JSON *)
  val read : json -> message

  (** Get the first name of the user who sent the message *)
  val get_sender_first_name : message -> string
  (** Get the username of the user who sent the message *)
  val get_sender_username : message -> string
  (** Get a formatted name for the user who sent the message: first name, with the username in parentheses if it exists *)
  val get_sender : message -> string
end

(** Actions that can be sent as user statuses *)
module ChatAction : sig
  (** Represents all recognized chat actions *)
  type action =
    | Typing
    | UploadPhoto
    | RecordVideo
    | UploadVideo
    | RecordAudio
    | UploadAudio
    | UploadDocument
    | FindLocation

  (** Gets the string representation of the action, for use in JSON *)
  val to_string : action -> string
end

module Update : sig
  (** Stores the info for updates to a chat/group *)
  type update = {
    update_id : int;
    message   : Message.message option
  }
  (** Create an `update` in a concise manner *)
  val create : update_id:int -> ?message:Message.message option -> unit -> update
  (** Read an `update` out of some JSON *)
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
    | ForwardMessage of int * int * int
    | SendChatAction of int * ChatAction.action
    | SendPhoto of int * string * string option * int option * (string Result.result -> action)
    | ResendPhoto of int * string * string option * int option
    | SendAudio of int * string * string * string * int option * (string Result.result -> action)
    | ResendAudio of int * string * string * string * int option
    | SendDocument of int * string * int option * (string Result.result -> action)
    | ResendDocument of int * string * int option
    | SendVoice of int * string * int option * (string Result.result -> action)
    | ResendVoice of int * string * int option
    | GetUpdates of (Update.update list Result.result -> action)
    | PeekUpdate of (Update.update Result.result -> action)
    | PopUpdate of (Update.update Result.result -> action)
    | Chain of action * action

  (** This type is used to represent available commands. The `name` field is the command's name (without a slash) and the `description` field is the description to be used in the help message. `run` is the function called when invoking the command. *)
  type command = {
    name            : string;
    description     : string;
    mutable enabled : bool;
    run             : Message.message -> action
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

  (** Forwards any message from one chat to another (can be same chat) *)
  val forward_message : chat_id:int -> from_chat_id:int -> message_id:int -> unit Result.result Lwt.t

  (** Send an action report to the chat, to show that a command will take some time *)
  val send_chat_action : chat_id:int -> action:ChatAction.action -> unit Result.result Lwt.t

  (** Send a new image file (jpeg/png) to a specified chat. Note that `photo` refers to the file's name to send. *)
  val send_photo : chat_id:int -> photo:string -> ?caption:string option -> reply_to:int option -> string Result.result Lwt.t

  (** Send an existing image file (jpeg/png) to a specified chat. Note that `photo` refers to the file's id on the Telegram servers. *)
  val resend_photo : chat_id:int -> photo:string -> ?caption:string option -> reply_to:int option -> unit Result.result Lwt.t

  (** Send a new audio file (mp3) to a specified chat. Note that `audio` refers to the file's name to send. *)
  val send_audio : chat_id:int -> audio:string -> performer:string -> title:string -> reply_to:int option -> string Result.result Lwt.t

  (** Send an existing audio file (mp3) to a specified chat. Note that `audio` refers to the file's id on the Telegram servers. *)
  val resend_audio : chat_id:int -> audio:string -> performer:string -> title:string -> reply_to:int option -> unit Result.result Lwt.t

  (** Send a new document file to a specified chat. Note that `document` refers to the file's name to send. *)
  val send_document : chat_id:int -> document:string -> reply_to:int option -> string Result.result Lwt.t

  (** Send an existing document file to a specified chat. Note that `document` refers to the file's id on the Telegram servers. *)
  val resend_document : chat_id:int -> document:string -> reply_to:int option -> unit Result.result Lwt.t

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
