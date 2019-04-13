(**[getachar ()] gets one char (no matter what it is) from the imput and then 
   returns. *)
let getachar () =
  let termio = Unix.tcgetattr Unix.stdin in
  let () =
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
      { termio with Unix.c_icanon = false } in
  let res = input_char stdin in
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
  res


open ANSITerminal
open Unix
(*outputs a list of lists of the form [y, x1, x2] ordered by y (greatest to least).
  y is a row that the snake appears on, x1 is the leftmost pixel and x2 is the 
  rightmost pixel of the snake in that row*)
(*let make_snake = 
  failwith "unimplemented"
  let make_apple = 
  failwith "unimplemented"
*)
(*returns a string of length num filled with whitespace*) 

let width = 78
let height = 30

type direction =
  |Up
  |Down
  |Left
  |Right

let rec whitespace num = 
  if num = 1 then " " 
  else " " ^ whitespace (num-1)

(**[get_snake_seg snake i] gets the [i]th segment of [snake]. *)
let get_snake_seg snake i=
  List.nth snake i

(**[get_seg_ycorr seg] gets the y corrdinate of the snake segment [seg]. *)
let get_seg_ycorr seg =
  List.nth seg 1

(**[get_seg_xcorr seg] gets the leftmost x corrdinate of the snake segment [seg]. *)
let get_seg_xcorr seg =
  List.nth seg 0

(** The format of each snake segment. *)
let snake_seg =
  "[]"

(*draws snake segment on one row; len number of repetitions*)
let rec draw_snake snake = 
  (* save_cursor(); *)
  match snake with
  |[] -> ()
  |h :: t -> set_cursor (get_seg_xcorr h) (get_seg_ycorr h);
    print_endline (snake_seg);draw_snake t
(* restore_cursor() *)

let draw_apple = 
  "o" 

(*checks if part of the snake is in the current row*)
let check_snake row snake = 
  if snake = [] then false
  else row = List.nth (List.nth snake 0) 1

(*checks if the apple is in the current row*)
let check_apple row apple = 
  row = snd(apple)

(**[check_eat apple snake] checks whether [snake] can eat the [apple]. *)
let check_eat apple snake =
  let head = get_snake_seg snake 0 in
  let head_x = get_seg_xcorr head in
  let head_y = get_seg_ycorr head in
  fst apple = head_x && (snd apple) = head_y
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
let make_board w h snake apple =

  print_endline (" " ^ draw_horiz_edge (w));
  draw_verti_edge w h;
  print_endline (" " ^ draw_horiz_edge (w));
  let pos = pos_cursor() in
  set_cursor (fst apple) (snd apple);
  print_string[red] (draw_apple);
  draw_snake snake;
  set_cursor (fst pos) ((snd pos)+1)

let snake_add_head (dir:direction) snake =
  match snake with
  |[] -> []
  |[x;y] :: t -> begin
      match dir with
      |Up -> [x; y-1] :: snake
      |Down -> [x; y+1] :: snake
      |Left -> [x-1; y] :: snake
      |Right -> [x+1; y] :: snake
    end
  |_ -> failwith "impossible"

let snake_remove_tail snake = 
  if List.length snake = 0 then snake else
    snake |> List.rev |> List.tl |> List.rev

(** [eat_apple apple snake] deletes apple when snake head is at same location 
  and creates a new apple in a random spot, then increases snake length by
  two (for now). *)
  (* when should this function be called?*)
let eat_apple apple snake dir =
  if check_eat apple snake 
    then let terminal_size = size() in 
    let new_apple = (min (2 + Random.int (width-2)) ((fst terminal_size)-1), 
      min (5 + Random.int (height-5)) ((snd terminal_size)-2)) in
    let length = List.length snake in 
    let last_snake = get_snake_seg snake (length-1) in 
    let last_snake_x = get_seg_xcorr last_snake in 
    let last_snake_y = get_seg_ycorr last_snake in 
    match dir with
    | Up -> let new_seg = [[last_snake_x; last_snake_y + 1]; 
      [last_snake_x; last_snake_y + 2]] in 
      let new_snake = snake @ new_seg in 
      make_board width height new_snake new_apple
    | Down -> let new_seg = [[last_snake_x; last_snake_y -1];
      [last_snake_x; last_snake_y - 2]]  in 
      let new_snake = snake @ new_seg in 
      make_board width height new_snake new_apple
    | Left -> let new_seg = [[last_snake_x + 1; last_snake_y];
      [last_snake_x + 2; last_snake_y]] in 
      let new_snake = snake @ new_seg in 
      make_board width height new_snake new_apple
    | Right -> let new_seg = [[last_snake_x -1; last_snake_y];
      [last_snake_x -2; last_snake_y]] in 
      let new_snake = snake @ new_seg in 
      make_board width height new_snake new_apple
  else make_board width height snake apple

(**[move snake apple sl old_dir new_dir] moves the snake every [sl] seconds.
   [old_dir] is the direction before a new direction button is pressed. 
   [new_dir] is the new direction depends on which button is pressed -- 
   "W" is Up, "S" is Down, "A" is Left, "D" is Right. *)
let rec move snake apple (sl:float) dir cursor_pos=
  sleepf(sl);
  (* erase Screen; *)
  set_cursor 1 (max ((snd cursor_pos)-height-2) 1);
  (* print_string[] ("\x1B[2J"); *)
  let new_snake = snake |> snake_add_head dir |> snake_remove_tail in
  if check_eat apple new_snake then eat_apple apple new_snake dir else
  make_board width height new_snake apple

let rec receive_input ()=
  let input = getachar() in
  match input with
  |'w' -> Up | 's' -> Down | 'a' -> Left | 'd' -> Right
  | _ -> receive_input();;

let play_game cursor_pos =
  let terminal_size = size() in
  let pro_ran () = 
    (min (2 + Random.int (width-2)) ((fst terminal_size)-1), min (5 + Random.int (height-5)) ((snd terminal_size)-2)) in 
  let rand = pro_ran() in 
  let snake = [[fst rand; snd rand]] in
  let apple = (pro_ran()) in
  (* print_endline ((string_of_int (fst terminal_size)) ^"  "^ (string_of_int (snd terminal_size))); *)
  make_board width height snake apple;
  (* move snake apple 0.8 Right Right   *)
  let rec play () =
    let input = receive_input() in 
    move snake apple 0.8 input cursor_pos;
    play () in
  play ()


let main () = 
  ANSITerminal.(print_string[red] "\n\ Welcome to Snake! Press enter to start \n");
  print_string[red] "> ";
  let cursor_pos = pos_cursor() in
  print_endline (string_of_int (fst cursor_pos) ^ "   " ^ string_of_int (snd cursor_pos));
  match read_line () with
  | exception _ -> ()
  | x -> play_game cursor_pos

let () = main ()