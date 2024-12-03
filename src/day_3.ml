let input =
  let channel = open_in "./inputs/day_3.txt" in
  try
    let length = in_channel_length channel in
    let content = really_input_string channel length in
    close_in channel;
    String.trim content
  with e ->
    close_in_noerr channel;
    raise e

let rec ops_1 (s : string) : (int * int) list =
  let remove_first = String.sub s 1 (String.length s - 1) in
  if String.length s <= 8 then
    (* Minimum character count is 8 - mul(x,y) has 8 characters *)
    []
  else if String.starts_with ~prefix:"mul(" s then
    match String.index_opt s ')' with
    | Some closing_ind -> (
        try
          let raw_args = String.sub s 4 (closing_ind - 4) in
          let args = String.split_on_char ',' raw_args in
          if List.length args = 2 then
            let ls = List.map String.trim args |> List.map int_of_string in
            let left, right = List.(nth ls 0, nth ls 1) in
            (left, right)
            :: ops_1
                 (String.sub s (closing_ind + 1)
                    (String.length s - closing_ind - 1))
          else ops_1 remove_first
        with _ -> ops_1 remove_first)
    | None -> ops_1 remove_first
  else ops_1 remove_first

let sum_1 =
  List.fold_left ( + ) 0 (List.map (fun (a, b) -> a * b) (ops_1 input))

let () = Printf.printf "Part 1: %d\n" sum_1

type bool_or_tuple = Do of bool | Op of (int * int)

let rec ops_2 (s : string) : bool_or_tuple list =
  let remove_first = String.sub s 1 (String.length s - 1) in
  if String.length s <= 8 then
    (* Minimum character count is 8 - mul(x,y) has 8 characters *)
    []
  else if String.starts_with ~prefix:"do(" s then
    Do true :: ops_2 (String.sub s 3 (String.length s - 3))
  else if String.starts_with ~prefix:"don't(" s then
    Do false :: ops_2 (String.sub s 6 (String.length s - 6))
  else if String.starts_with ~prefix:"mul(" s then
    match String.index_opt s ')' with
    | Some closing_ind -> (
        try
          let raw_args = String.sub s 4 (closing_ind - 4) in
          let args = String.split_on_char ',' raw_args in
          if List.length args = 2 then
            let ls = List.map String.trim args |> List.map int_of_string in
            let left, right = List.(nth ls 0, nth ls 1) in
            Op (left, right)
            :: ops_2
                 (String.sub s (closing_ind + 1)
                    (String.length s - closing_ind - 1))
          else ops_2 remove_first
        with _ -> ops_2 remove_first)
    | None -> ops_2 remove_first
  else ops_2 remove_first

let rec sum_2 (ops : bool_or_tuple list) (dos : bool) : int =
  match (ops, dos) with
  | [], _ -> 0
  | Op (o1, o2) :: os, true -> (o1 * o2) + sum_2 os dos
  | Op _ :: os, false -> sum_2 os dos
  | Do new_do :: os, _ -> sum_2 os new_do

let sum_2 = sum_2 (ops_2 input) true
let () = Printf.printf "Part 2: %d\n" sum_2
