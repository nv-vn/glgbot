open Lwt
open Cohttp
open Cohttp_lwt_unix

open Batteries

open Yojson.Safe

let rec get_field target = function
  | `Assoc [] -> `Null
  | `Assoc (x::xs) when fst x = target -> snd x
  | `Assoc (x::xs) -> get_field target (`Assoc xs)
  | _ -> `Null

let the_list = function
  | `List xs -> xs
  | _ -> []

let the_string = function
  | `String x -> x
  | _ -> ""

let fix url =
  if String.ends_with url ".gifv" then
    String.sub url 0 (String.length url - 1)
  else url

let (>>) f g x = f (g x)

let get_images ?(num = 25) subreddit =
  let url = "https://api.reddit.com/r/" ^ subreddit ^ "/.json?limit=" ^ string_of_int num in
  let headers = Header.init_with "User-Agent" "glgbot" in
  Client.get ~headers (Uri.of_string url) >>= fun (resp, body) ->
  Cohttp_lwt_body.to_string body >>= fun json ->
  let obj = Yojson.Safe.from_string json in
  let data = get_field "data" obj in
  let children = the_list @@ get_field "children" data in
  let data_children = List.map (get_field "data") children in
  let imgur_children = List.filter (fun post -> get_field "domain" post = `String "i.imgur.com") data_children in
  return @@ List.map (fix >> the_string >> get_field "url") imgur_children
