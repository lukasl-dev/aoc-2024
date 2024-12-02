let input =
  let channel = open_in "./inputs/day_1.txt" in
  try
    let length = in_channel_length channel in
    let content = really_input_string channel length in
    close_in channel;
    content
  with e ->
    close_in_noerr channel;
    raise e

let lines : string list = String.split_on_char '\n' input

let rows : (int * int) list =
  List.filter (fun line -> String.length line > 0) lines
  |> List.map (fun line ->
         ( int_of_string (String.sub line 0 5),
           int_of_string (String.sub line 8 5) ))

let (left, right) : int list * int list =
  ( List.sort compare (List.map (fun (row, _) -> row) rows),
    List.sort compare (List.map (fun (_, col) -> col) rows) )

(* Part 1 *)

let total_distance : int =
  List.combine left right
  |> List.map (fun (a, b) -> abs (a - b))
  |> List.fold_left ( + ) 0

let () = Printf.printf "Part 1: %d\n" total_distance

(* Part 2 *)

let rec occurrences o right =
  match right with
  | [] -> 0
  | x :: xs -> if x = o then 1 + occurrences o xs else occurrences o xs

let total_similarity : int =
  List.map (fun a -> a * occurrences a right) left |> List.fold_left ( + ) 0

let () = Printf.printf "Part 2: %d\n" total_similarity
