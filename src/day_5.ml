let input =
  let channel = open_in "./inputs/day_5.txt" in
  try
    let length = in_channel_length channel in
    let content = really_input_string channel length in
    close_in channel;
    String.trim content
  with e ->
    close_in_noerr channel;
    raise e

let sections (s : string) : string * string =
  let rec find_section_divider (i : int) : int =
    if i >= String.length s - 1 then failwith "No divider found"
    else if s.[i] = '\n' && s.[i + 1] = '\n' then i
    else find_section_divider (i + 1)
  in
  let divider_index = find_section_divider 0 in
  let top = String.sub s 0 divider_index in
  let bottom =
    String.sub s (divider_index + 2) (String.length s - divider_index - 2)
  in
  (top, bottom)

type rules = (int * int) list

let parse_rules (s : string) : (int * int) list =
  let parse_rule (s : string) : int * int =
    let split = String.split_on_char '|' s in
    if List.length split <> 2 then failwith "Invalid rule"
    else
      let a = int_of_string (List.nth split 0) in
      let b = int_of_string (List.nth split 1) in
      (a, b)
  in
  let lines =
    String.split_on_char '\n' s
    |> List.map String.trim
    |> List.filter (fun s -> String.length s > 0)
  in
  lines |> List.map parse_rule

type input = int array array

let parse_input (s : string) : int array array =
  let parse_line (line : string) : int array =
    let split = String.split_on_char ',' line in
    split |> List.map int_of_string |> Array.of_list
  in
  let lines =
    String.split_on_char '\n' s
    |> List.map String.trim
    |> List.filter (fun s -> String.length s > 0)
    |> List.map parse_line
  in
  Array.of_list lines

let parse (s : string) : (int * int) list * int array array =
  let raw_rules, raw_in = sections s in
  let rules = parse_rules raw_rules in
  let in_input = parse_input raw_in in
  (rules, in_input)

let is_valid (input : int array) (rules : rules) : bool =
  let page_indices =
    let mapping = Hashtbl.create (Array.length input) in
    Array.iteri (fun idx page -> Hashtbl.add mapping page idx) input;
    mapping
  in
  List.for_all
    (fun (x, y) ->
      if Hashtbl.mem page_indices x && Hashtbl.mem page_indices y then
        Hashtbl.find page_indices x < Hashtbl.find page_indices y
      else true (* Rule doesn't apply if either page is missing *))
    rules

let sum_1 (input : input) (rules : rules) : int =
  input
  |> Array.map (fun line ->
         if is_valid line rules then line.(Array.length line / 2) else 0)
  |> Array.fold_left ( + ) 0

let example_1 =
  String.trim
    "\n\
     47|53\n\
     97|13\n\
     97|61\n\
     97|47\n\
     75|29\n\
     61|13\n\
     75|53\n\
     29|13\n\
     97|29\n\
     53|29\n\
     61|53\n\
     97|53\n\
     61|29\n\
     47|13\n\
     75|47\n\
     97|75\n\
     47|61\n\
     75|61\n\
     47|29\n\
     75|13\n\
     53|13\n\n\
     75,47,61,53,29\n\
     97,61,53,29,13\n\
     75,29,13\n\
     75,97,47,61,53\n\
     61,13,29\n\
     97,13,75,29,47\n"

let in_rules, in_input = parse input
let ex_rules, ex_in = parse example_1

let () =
  print_endline "Part 1:";
  Printf.printf "> Example: %d\n" (sum_1 ex_in ex_rules);
  Printf.printf "> Input: %d\n" (sum_1 in_input in_rules)

(* Part 2 *)

let compare_pages (rules : rules) (x : int) (y : int) : int =
  if x = y then 0
  else if List.mem (x, y) rules then -1
  else if List.mem (y, x) rules then 1
  else 0

let correct_order (update : int array) (rules : rules) : int array =
  let pages = Array.to_list update in
  let compare x y = compare_pages rules x y in
  let sorted_pages = List.stable_sort compare pages in
  Array.of_list sorted_pages

let sum_2 (input : input) (rules : rules) : int =
  input
  |> Array.fold_left
       (fun acc line ->
         if not (is_valid line rules) then
           let corrected = correct_order line rules in
           let middle_page = corrected.(Array.length corrected / 2) in
           acc + middle_page
         else acc)
       0

let () =
  print_endline "\nPart 2:";
  Printf.printf "> Example: %d\n" (sum_2 ex_in ex_rules);
  Printf.printf "> Input: %d\n" (sum_2 in_input in_rules)
