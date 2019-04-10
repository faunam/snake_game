open ANSITerminal

(*outputs a list of lists of the form [y, x1, x2] ordered by y (greatest to least).
  y is a row that the snake appears on, x1 is the leftmost pixel and x2 is the 
  rightmost pixel of the snake in that row*)
(* let make_snake = 
   failwith "boop"

   let make_apple = 
   failwith "bep" *)

let rec whitespace num = 
  if num == 1 then " " 
  else " " ^ whitespace (num-1)

(*draws snake segment on one row; len number of repetitions*)
let rec draw_snake_seg len = 
  if len == 1 then "o" 
  else "o" ^ draw_snake_seg (len-1)

let draw_apple = 
  "u"

let check_snake row snake = 
  if snake == [] then false
  else row == List.nth (List.nth snake 0) 0

let check_apple row apple = 
  row == snd(apple)

(**snake is a list (described in make_snake), apple is a tuple (x, y) of apple location *)
let rec draw_internal w l snake apple = 
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

  if l == 1 then print_endline row
  else print_endline row; draw_internal w (l-1) next_snake apple

let rec draw_vert_edge w = 
  if w == 1 then "-" 
  else "-" ^ draw_vert_edge (w-1)

(*w and l are the width and height of the playable area. so 0,0 is the first playable
  pixel and w,l is the last*)
let make_board w l snake apple =
  print_endline (" " ^ draw_vert_edge (w));
  draw_internal w l snake apple;
  print_endline (" " ^ draw_vert_edge (w))

let play_game =
  make_board 20 10 [[1;1;1]] (7,8)

let main () = 
  ANSITerminal.(print_string[red] "\n\ Welcome to Snake! Press enter to start. 
    \n");
  match read_line () with
  | exception End_of_file -> ()
  | _ -> play_game 

let () = main ()