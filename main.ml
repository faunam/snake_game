(**[getachar ()] gets one char (no matter what it is) from the imput and then 
   returns. *)
let getachar () =
  let termio = Unix.tcgetattr Unix.stdin in
  let () =
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
      { termio with Unix.c_icanon = false } in
  let res = input_char stdin in
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
  res;;


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

(**[snake_update t old_dir new_dir turnover cur_pos] updates the snake position.
   [old_dir] is the direction before a new direction button is pressed.
   [turnover] is the turn point of the snake, exists only if [old_dir] is not 
   equail to [new_dir]. 
   [cur_pos] is the segment accumulater. *)
let rec snake_update snake (old_dir:direction) (new_dir:direction) turnover cur_pos =
  (* if turnover = (List.length snake) - 1 then snake else *)
  match snake with
  | [] -> snake
  | [hx; hy] :: t -> begin
      let tail () = (snake_update t old_dir new_dir turnover (cur_pos+1)) in
      match (old_dir, new_dir) with
      | (Up, Up) -> [hx;hy-1] :: tail()
      | (Down, Down) -> [hx;hy+1] :: tail()
      | (Left, Left) -> [hx-1;hy] :: tail()
      | (Right, Right) -> [hx+1;hy] :: tail()
      | (Left, n) -> 
        if cur_pos <= turnover then if n = Up then [hx;hy-1] :: tail()  
          else if n = Down then [hx;hy+1] :: tail() else failwith "impossible"
        else [hx-1;hy] :: tail()
      | (Right, n) -> 
        if cur_pos <= turnover then if n = Up then [hx;hy-1] :: tail()  
          else if n = Down then [hx;hy+1] :: tail() else failwith "impossible"
        else [hx+1;hy] :: tail()
      | (Up, n) -> 
        if cur_pos <= turnover then if n = Left then [hx-1;hy] :: tail()  
          else if n = Right then [hx+1;hy] :: tail() else failwith "impossible"
        else [hx;hy-1] :: tail()
      | (Down, n) -> 
        if cur_pos <= turnover then if n = Left then [hx-1;hy] :: tail()  
          else if n = Right then [hx+1;hy] :: tail() else failwith "impossible"
        else [hx;hy+1] :: tail()
    end
  |_-> failwith "impossiable"

(**[move snake apple sl old_dir new_dir] moves the snake every [sl] seconds.
   [old_dir] is the direction before a new direction button is pressed. 
   [new_dir] is the new direction depends on which button is pressed -- 
   "W" is Up, "S" is Down, "A" is Left, "D" is Right. *)
let rec move snake apple (sl:float) (old_dir:direction) (new_dir:direction)=
  sleepf(sl);
  erase Above;
  let new_snake = snake_update snake old_dir new_dir 0 0 in
  make_board width height new_snake apple

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

let play_game () =
  let terminal_size = size() in
  let pro_ran () = 
    (min (2 + Random.int (width-2)) ((fst terminal_size)-1), min (5 + Random.int (height-5)) ((snd terminal_size)-2)) in 
  let rand = pro_ran() in 
  let snake = [[fst rand; snd rand]] in
  let apple = (pro_ran()) in
  print_endline ((string_of_int (fst terminal_size)) ^"  "^ (string_of_int (snd terminal_size)));
  make_board width height snake apple;
  move snake apple 0.8 Right Right  


let main () = 
  ANSITerminal.(print_string[red] "\n\ Welcome to Snake! Press enter to start \n");
  print_string[red] "> ";
  match read_line () with
  | exception _ -> ()
  | x -> play_game ()
(* print_endline (string_of_int (fst up_left) ^ "   " ^ string_of_int (snd up_left)) *)

let () = main ()