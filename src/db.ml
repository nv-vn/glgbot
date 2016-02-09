open Gensqlite_tools

let db = Sqlite3.db_open "data/glg.db"

module Quotes = struct
  let (_, put)   = [%gensqlite db "INSERT INTO quotes (who, quote) VALUES (%s{who}, %s{quote})"]
  let (_, get)   = [%gensqlite db "SELECT @s{who}, @s{quote}, @d{timestamp} FROM quotes WHERE ROWID = %d{index}"]
  let (_, size)  = [%gensqlite db "SELECT count(*) AS @d{c} FROM quotes"]
  let (_, clear) = [%gensqlite db "DELETE FROM quotes"]

  let get_random () =
    let max = match size () with
      | [] -> 0
      | x::_ -> x in
    let index = if max > 1 then (Random.int (max - 1)) + 1 else 1 in (* Range is 0...count *)
    match get ~index () with
    | [] -> print_endline ("Index " ^ string_of_int index ^ " not found in database");
    ("Error", "Error", 0)
    | x::_ -> x
end

module Jukebox = struct
  let (_, put)   = [%gensqlite db "INSERT INTO jukebox (title, performer, file_id) VALUES (%s{title}, %s{performer}, %s{file_id})"]
  let (_, get)   = [%gensqlite db "SELECT @s{title}, @s{performer}, @s{file_id} FROM jukebox WHERE title LIKE %s{title}"]
  let (_, all)   = [%gensqlite db "SELECT @s{title}, @s{performer} FROM jukebox"]
  let (_, size)  = [%gensqlite db "SELECT count(*) AS @d{c} FROM jukebox"]
  let (_, clear) = [%gensqlite db "DELETE FROM jukebox"]

  let search ~title =
    match get ~title () with
    | [] -> ("Unknown", "Unknown", "Unknown")
    | x::_ -> x

  let list () =
    List.map (fun (title, performer) -> performer ^ " - " ^ title) (all ())
end
