open ANSITerminal

(*outputs a list of lists of the form [y, x1, x2] ordered by y (greatest to least).
  y is a row that the snake appears on, x1 is the leftmost pixel and x2 is the 
  rightmost pixel of the snake in that row*)
(*let make_snake = 
  failwith "unimplemented"

  let make_apple = 
  failwith "unimplemented"
*)
(*returns a string of length num filled with whitespace*)  
let rec whitespace num = 
  if num = 1 then " " 
  else " " ^ whitespace (num-1)

(**[get_snake_seg snake i] gets the [i]th segment of [snake]. *)
let get_snake_seg snake i=
  List.nth snake i

(**[get_seg_ycorr seg] gets the y corrdinate of the snake segment [seg]. *)
let get_seg_ycorr seg =
  List.nth seg 0

(**[get_seg_xcorr seg] gets the leftmost x corrdinate of the snake segment [seg]. *)
let get_seg_xcorr seg =
  List.nth seg 1

(** The format of each snake segment. *)
let snake_seg =
  "[]"

let rec draw_snake_seg snake len i=
  let seg = get_snake_seg snake i in
  set_cursor (get_seg_xcorr seg) (get_seg_ycorr seg);
  if i = len-1 then print_endline (snake_seg)
  else (print_endline (snake_seg); draw_snake_seg snake len (i+1))

(*draws snake segment on one row; len number of repetitions*)
let draw_snake snake = 
  (* save_cursor(); *)
  let len = List.length snake in
  draw_snake_seg snake len 0
(* restore_cursor() *)

let draw_apple = 
  (*TODO: implement correctly*)
  "o" (*for testing purposes*)
(*failwith "unimplemented"*)

(*checks if part of the snake is in the current row*)
let check_snake row snake = 
  if snake = [] then false
  else row = List.nth (List.nth snake 0) 0

(*checks if the apple is in the current row*)
let check_apple row apple = 
  row = snd(apple)

(**snake is a list (described in make_snake), apple is a tuple (x, y) of apple 
   location this may not be the best way to implement snake and apple so we can 
   change it if necessary. Maybe they can be included in the state variable st? 
   I just added it now so that the function would return unit. 

   Problem: the function prints forever right now and i can't figure out why*)

(*this function goes row by row and if the snake or the apple are in the current row, 
  it draws them and adds white space around them, and the board borders.*)
let rec draw_verti_edge w h = 
  let row = "|" ^ (whitespace (w)) ^ "|"
  in
  if h = 1 then (print_endline row; print_endline) 
  else (print_endline row; draw_verti_edge w (h-1))

(**[draw_horiz_edge w] draws the horizontal bondrary of the board. *)
let rec draw_horiz_edge w  = 
  if w = 1 then "-" 
  else "-" ^ draw_horiz_edge (w-1)

(*w and h are the width and height of the playable area. so 0,0 is the first 
  playable pixel and w,l is the last*)
let make_board st w h snake apple =
  print_endline (" " ^ draw_horiz_edge (w));
  draw_verti_edge w h;
  print_endline (" " ^ draw_horiz_edge (w));
  save_cursor();
  set_cursor (fst apple) (snd apple);
  print_endline (draw_apple);
  draw_snake snake;
  restore_cursor()



let play_game arg =
  (*testing make_board*)
  make_board () 78 30 [[2;2];[2;4];[2;6]] (7,8)

let main () = 
  ANSITerminal.(print_string[red] "\n\ Welcome to Snake! Press enter to start \n");
  print_string[red] "> ";
  match read_line () with
  | exception End_of_file -> ()
  | x -> play_game ()

let () = main ()