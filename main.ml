open ANSITerminal

(*outputs a list of lists of the form [y, x1, x2] ordered by y (greatest to least).
  y is a row that the snake appears on, x1 is the leftmost pixel and x2 is the 
  rightmost pixel of the snake in that row*)
let make_snake = 
  failwith "unimplemented"

let make_apple = 
  failwith "unimplemented"

(*returns a string of length num filled with whitespace*)  
let rec whitespace num = 
  if num == 1 then " " 
  else " " ^ whitespace (num-1)

(*TODO: implement correctly*)
(*draws snake segment on one row; len number of repetitions*)
let rec draw_snake_seg len = 
  if len == 1 then "o" 
  else "o" ^ draw_snake_seg (len-1) (*for testing purposes*)
(*failwith "unimplemented"*)

let draw_apple = 
  (*TODO: implement correctly*)
  "u" (*for testing purposes*)
(*failwith "unimplemented"*)

(*checks if part of the snake is in the current row*)
let check_snake row snake = 
  if snake == [] then false
  else row == List.nth (List.nth snake 0) 0

(*checks if the apple is in the current row*)
let check_apple row apple = 
  row == snd(apple)

(**snake is a list (described in make_snake), apple is a tuple (x, y) of apple location 
   this may not be the best way to implement snake and apple so we can change it if
   necessary. Maybe they can be included in the state variable st? I just added it 
   now so that the function would return unit. 

   Problem: the function prints forever right now and i can't figure out why*)

(*this function goes row by row and if the snake or the apple are in the current row, 
  it draws them and adds white space around them, and the board borders.*)
let rec draw_internal st w l snake apple = 
  let row = 
    match (check_apple l apple , check_snake l snake) with
    | (false, false) -> "|" ^ (whitespace (w)) ^ "|"
    | (true, false) -> "|" ^ (whitespace (fst(apple)-1)) ^ (draw_apple) ^ (whitespace (w-fst(apple))) ^ "|"
    | (x, true) -> 
      let snake_x1 = List.nth (List.nth snake 0) 1 in 
      let snake_x2 = List.nth (List.nth snake 0) 2 in
      let len = snake_x2 - snake_x1 + 1 in
      match x with 
      | false -> "|" ^ (whitespace (snake_x1-1)) ^ (draw_snake_seg len) ^ 
                 (whitespace (w-snake_x2)) ^ "|"
      | true ->
        if fst(apple) < snake_x1 then 
          "|" ^ (whitespace (fst(apple)-1)) ^ (draw_apple) ^ (whitespace (snake_x1-fst(apple)-1)) ^ 
          (draw_snake_seg len) ^ (whitespace (w-snake_x2)) ^ "|"
        else 
          "|" ^ (whitespace (snake_x1-1)) ^ (draw_snake_seg len) ^ (whitespace (fst(apple)-snake_x2-1)) 
          ^ (draw_apple) ^ (whitespace (w-fst(apple))) ^ "|"
  in 

  let next_snake = match snake with
    | [] -> []
    | h :: t -> if check_snake l snake then t else snake in

  if l == 1 then (print_endline row; st) (*for some reason this never triggers*)
  else print_endline row; draw_internal st w (l-1) next_snake apple

let rec draw_vert_edge w = 
  if w == 1 then "-" 
  else "-" ^ draw_vert_edge (w-1)

(*w and l are the width and height of the playable area. so 0,0 is the first playable
  pixel and w,l is the last*)
let make_board st w l snake apple =
  print_endline (" " ^ draw_vert_edge (w));
  draw_internal st w l snake apple;
  print_endline (" " ^ draw_vert_edge (w))

let play_game arg =
  (*testing make_board*)
  make_board () 20 10 [[2;2;2]] (7,8)

let main () = 
  ANSITerminal.(print_string[red] "\n\ Welcome to Snake! Press enter to start \n");
  print_string[red] "> ";
  match read_line () with
  | exception End_of_file -> ()
  | x -> play_game ()

let () = main ()