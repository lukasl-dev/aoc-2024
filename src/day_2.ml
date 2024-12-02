let input =
  let channel = open_in "./inputs/day_2.txt" in
  try
    let length = in_channel_length channel in
    let content = really_input_string channel length in
    close_in channel;
    content
  with e ->
    close_in_noerr channel;
    raise e

let lines : string list = String.split_on_char '\n' input

let rows : int array list =
  List.filter (fun line -> String.length line > 0) lines
  |> List.map (fun line ->
         String.split_on_char ' ' line
         |> List.map int_of_string |> Array.of_list)

(* Part 1 *)

let is_safe_1 row : bool =
  let check asc i j =
    let current, next = (row.(i), row.(j)) in
    let delta = abs (current - next) in
    (if asc then current <= next else current >= next)
    && delta >= 1 && delta <= 3
  in

  let rec aux asc i : bool =
    if i >= Array.length row - 1 then true
    else check asc i (i + 1) && aux asc (i + 1)
  in
  aux true 0 || aux false 0

let safe_count_1 : int = rows |> List.filter is_safe_1 |> List.length
let () = Printf.printf "Part 1: %d\n" safe_count_1

(* Part 2 *)

let remove_at i arr =
  if i < 0 || i >= Array.length arr then invalid_arg "Index out of bounds"
  else
    Array.init
      (Array.length arr - 1)
      (fun j -> if j < i then arr.(j) else arr.(j + 1))

let is_safe_2 row : bool =
  if is_safe_1 row then true
  else
    let len = Array.length row in
    let rec try_remove i =
      if i >= len then false
      else
        let new_row = remove_at i row in
        if is_safe_1 new_row then true else try_remove (i + 1)
    in
    try_remove 0

let safe_count_2 : int = rows |> List.filter is_safe_2 |> List.length
let () = Printf.printf "Part 2: %d\n" safe_count_2
