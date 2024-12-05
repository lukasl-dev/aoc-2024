let input =
  let channel = open_in "./inputs/day_4.txt" in
  try
    let length = in_channel_length channel in
    let content = really_input_string channel length in
    close_in channel;
    String.trim content
  with e ->
    close_in_noerr channel;
    raise e

let example_1 =
  String.trim
    "\n\
     ....XXMAS.\n\
     .SAMXMS...\n\
     ...S..A...\n\
     ..A.A.MS.X\n\
     XMASAMX.MM\n\
     X.....XA.A\n\
     S.S.S.S.SS\n\
     .A.A.A.A.A\n\
     ..M.M.M.MM\n\
     .X.X.XMASX\n"

type mat = char array array
type vec = { x : int; y : int }

let string_to_mat (s : string) : mat =
  let line_to_array (line : string) : char array =
    Array.init (String.length line) (fun ind -> line.[ind])
  in
  String.split_on_char '\n' s
  |> List.map String.trim
  |> List.filter (fun s -> String.length s > 0)
  |> List.map line_to_array |> Array.of_list

let print_mat (mat : mat) =
  Array.iter
    (fun line ->
      Array.iter (Printf.printf "%c") line;
      Printf.printf "\n")
    mat

let add_vecs (v : vec) (u : vec) : vec = { x = v.x + u.x; y = v.y + u.y }

let inside_mat (mat : mat) (pos : vec) : bool =
  pos.y >= 0
  && pos.y < Array.length mat
  && pos.x >= 0
  && pos.x < Array.length mat.(pos.y)

let mat_to_pos (mat : mat) : vec list =
  let rec aux (pos : vec) : vec list =
    if pos.y >= Array.length mat then []
    else if pos.x >= Array.length mat.(pos.y) then aux { x = 0; y = pos.y + 1 }
    else
      let rem = aux { x = pos.x + 1; y = pos.y } in
      pos :: rem
  in
  aux { x = 0; y = 0 }

let possible_dirs : vec list =
  [
    (* Up *)
    { x = 0; y = -1 };
    (* Down *)
    { x = 0; y = 1 };
    (* Left *)
    { x = -1; y = 0 };
    (* Right *)
    { x = 1; y = 0 };
    (* Up-Left *)
    { x = -1; y = -1 };
    (* Up-Right *)
    { x = 1; y = -1 };
    (* Bottom-Left *)
    { x = -1; y = 1 };
    (* Bottom-Right *)
    { x = 1; y = 1 };
  ]

(* Part 1 *)

let check_word (mat : mat) (pos : vec) (dir : vec) (word : string) : bool =
  let len = String.length word in
  let rec aux i current_pos =
    if i >= len then true
    else if not (inside_mat mat current_pos) then false
    else if mat.(current_pos.y).(current_pos.x) <> word.[i] then false
    else aux (i + 1) (add_vecs current_pos dir)
  in
  aux 0 pos

let is_xmas (mat : mat) (pos : vec) : int =
  let word = "XMAS" in
  possible_dirs
  |> List.filter (fun dir -> check_word mat pos dir word)
  |> List.length

let count_xmas (mat : mat) : int =
  mat_to_pos mat |> List.map (is_xmas mat) |> List.fold_left ( + ) 0

let example_1_mat : mat = string_to_mat example_1
let input_mat : mat = string_to_mat input

let () =
  print_endline "Part 1:";
  Printf.printf "> Example: %d\n" (count_xmas example_1_mat);
  Printf.printf "> Input: %d\n" (count_xmas input_mat)

(* Part 2 *)

let dir_tl : vec = { x = -1; y = -1 }
let dir_br : vec = { x = 1; y = 1 }
let dir_tr : vec = { x = 1; y = -1 }
let dir_bl : vec = { x = -1; y = 1 }

let is_x_mas (mat : mat) (pos : vec) : bool =
  let inside = inside_mat mat in
  if not (inside pos) then false
  else
    let ch = mat.(pos.y).(pos.x) in
    if ch <> 'A' then false
    else
      let tl = add_vecs pos dir_tl in
      let br = add_vecs pos dir_br in
      let tr = add_vecs pos dir_tr in
      let bl = add_vecs pos dir_bl in
      if not (inside tl && inside br && inside tr && inside bl) then false
      else
        let ch_tl = mat.(tl.y).(tl.x) in
        let ch_br = mat.(br.y).(br.x) in
        let ch_tr = mat.(tr.y).(tr.x) in
        let ch_bl = mat.(bl.y).(bl.x) in
        let diag1 =
          String.make 1 ch_tl ^ String.make 1 ch ^ String.make 1 ch_br
        in
        let diag2 =
          String.make 1 ch_tr ^ String.make 1 ch ^ String.make 1 ch_bl
        in
        let is_mas s = s = "MAS" || s = "SAM" in
        is_mas diag1 && is_mas diag2

let count_x_mas (mat : mat) : int =
  mat_to_pos mat
  |> List.map (fun pos -> if is_x_mas mat pos then 1 else 0)
  |> List.fold_left ( + ) 0

let example_2 =
  String.trim
    "\n\
     .M.S......\n\
     ..A..MSMS.\n\
     .M.S.MAA..\n\
     ..A.ASMSM.\n\
     .M.S.M....\n\
     ..........\n\
     S.S.S.S.S.\n\
     .A.A.A.A..\n\
     M.M.M.M.M.\n\
     ..........\n"

let example_2_mat : mat = string_to_mat example_2

let () =
  print_endline "\nPart 2:";
  print_mat example_2_mat;
  let example_2_count = count_x_mas example_2_mat in
  Printf.printf "> Example 2 Count: %d\n" example_2_count;
  let input_2_count = count_x_mas input_mat in
  Printf.printf "> Input 2 Count: %d\n" input_2_count
