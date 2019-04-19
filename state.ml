(**[getachar sl] gets one char (no matter what it is) from the imput and then 
    returns. *)
let getachar sl =
  let termio = Unix.tcgetattr Unix.stdin in
  let () =
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
      { termio with Unix.c_icanon = false; Unix.c_vmin = 0; Unix.c_vtime = sl} in
  let res = input_char stdin in
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
  res

(**[reset_terminal ()] resets the terminal to the state before the game begins.
*)
let reset_terminal () = 
  let termio = Unix.tcgetattr Unix.stdin in
  let new_ter =
    { termio with Unix.c_icanon = true; Unix.c_vmin = 1; Unix.c_vtime=0} in
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN new_ter

open ANSITerminal
open Unix

(** Width of the canvas. *)
let width = 58

(** Height of the canvas. *)
let height = 20

(** Height of the terminal. *)
let ter_hei = height + 6

(** Width of the terminal. *)
let ter_wid = width + 2

(** Snake movement direction*)
type direction =
  |Up
  |Down
  |Left
  |Right

(** [whitespace num] is a string composed of only whitespace with [num]. *)
let rec whitespace num = 
  if num = 0 then "" else
  if num = 1 then " " 
  else " " ^ whitespace (num-1)

(**[get_snake_seg snake i] gets the [i]th segment of [snake]. *)
let get_snake_seg snake i=
  List.nth snake i

(**[get_seg_ycorr seg] gets the y corrdinate of the snake segment [seg]. *)
let get_seg_ycorr seg =
  List.nth seg 1

(**[get_seg_xcorr seg] gets the leftmost x corrdinate of the snake segment 
    [seg]. *)
let get_seg_xcorr seg =
  List.nth seg 0

(** The format of each snake segment. *)
let snake_seg =
  "[]"
(**[get_snake_head snake] gets the snake head in tuple format. *)
let get_snake_head snake =
  let head = List.hd snake in
  (List.hd head, List.nth head 1)

(** [draw_snake snake] draws [snake].*)
let rec draw_snake snake = 
  match snake with
  |[] -> ()
  |h :: t -> set_cursor (get_seg_xcorr h) (get_seg_ycorr h);
    print_string[green] (snake_seg);draw_snake t

(** [draw_snake snake] draws [snake].*)
let rec draw_snake snake = 
  match snake with
  |[] -> ()
  |h :: t -> set_cursor (get_seg_xcorr h) (get_seg_ycorr h);
    print_string[green] (snake_seg);draw_snake t

(** The format of the apple. *)
let draw_apple = 
  "o" 

(** y cordinate of the top line of the canvas.*)
let row_top = 4

(** [produce_random_pos] produces a random position inside the canvas. *)
let produce_random_pos ()=
  ((3 + Random.int (width-3)), (5 + Random.int (height-5)))

(**[get_all_enem_pos is_hor pos num acc] are all positions of enemies with 
   starting position [pos] and length [num]. if [is_hor] is true, then enemies 
   are horizontal otherwise vertical. *)
let rec get_all_enem_pos is_hor pos num acc=
  if num > 0 then
    let (x,y) = pos in
    let acc' = (x,y)::acc in
    if is_hor then get_all_enem_pos is_hor ((x+1),y) (num-1) acc'
    else get_all_enem_pos is_hor (x, (y+1)) (num-1) acc'
  else acc

(**[check_conflicts snake apple enemies] checks whether the [snake] head or 
   [apple] overlaps with [enemies]. *)
let check_conflicts snake apple enemies =
  let snake_head = get_snake_head snake in
  (List.mem snake_head enemies || List.mem apple enemies)

(**[make_enemies snake apple is_hor enemies] are positions of all enemies. *)
let rec make_enemies snake apple is_hor enemies=
  let rand = 1+Random.int 5 in
  (* new-produced enemies position *)
  let enem_pos = produce_random_pos() in
  (* all enemies positions *)
  let enemies_pos = get_all_enem_pos is_hor enem_pos rand enemies in
  if check_conflicts snake apple enemies_pos 
  (*cannot make enemies at the same positon as snake head or apple*)
  then make_enemies snake apple is_hor enemies
  else enemies_pos

(**[draw_verti_edge w h] drows the vertical boundaries with height [h]. 
    The distance between two vertical lines is [w]. *)
let rec draw_verti_edge w h = 
  let row = "|" ^ (whitespace (w)) ^ "|"
  in
  if h = 1 then (print_endline row; print_endline) 
  else (print_endline row; draw_verti_edge w (h-1))

(**[draw_horiz_edge w] draws the horizontal bondrary of the board with 
   length [w]. *)
let rec draw_horiz_edge w  = 
  if w = 1 then "-" 
  else "-" ^ draw_horiz_edge (w-1)

(**[draw_enemies enemies] draws the [enemies] on the board. *)
let rec draw_enemies = function
  | [] -> ()
  | (x, y) :: t -> set_cursor x y;
    print_string[on_black] (" "); draw_enemies t

(**[make_board w h snake apple] draws the canvas with [snake] and [apple] 
    inside. [w] and [h] are the width and height of the canvas. *)
let make_board w h snake apple enemies=

  print_endline (" " ^ draw_horiz_edge (w));
  draw_verti_edge w h;
  print_endline (" " ^ draw_horiz_edge (w));
  let pos = (1,26) in
  set_cursor (fst apple) (snd apple);
  print_string[red] (draw_apple);
  draw_snake snake;
  draw_enemies enemies;
  set_cursor (fst pos) ((snd pos)+1);
  print_string[blue] ("  Score: " ^ string_of_int(List.length snake) ^ 
                      whitespace(w-10));
  set_cursor (fst pos) ((snd pos)+4)

(**[check_eat apple snake] checks whether [snake] can eat the [apple]. *)
let check_eat apple snake =
  let head = get_snake_seg snake 0 in
  let head_x = get_seg_xcorr head in
  let head_y = get_seg_ycorr head in
  let apple_x = fst apple in
  let apple_y = snd apple in
  apple_x = head_x &&  apple_y = head_y ||
  (apple_x = (head_x+1) && apple_y = head_y)

(**[snake_add_head dir snake] adds a new segment to the head of [snake] 
    following the direction [dir]. *)
let snake_add_head (dir:direction) snake =
  match snake with
  |[] -> []
  |[x;y] :: t -> begin
      match dir with
      |Up -> [x; y-1] :: snake
      |Down -> [x; y+1] :: snake
      |Left -> [x-2; y] :: snake
      |Right -> [x+2; y] :: snake
    end
  |_ -> failwith "impossible" (** shouldn't be executed*)

(**[snake_remove_tail snake] removes the last segment of [snake]. *)
let snake_remove_tail snake = 
  if List.length snake = 0 then snake else
    snake |> List.rev |> List.tl |> List.rev

(**[produce_random_pos] produces a random position inside the canvas. *)
let produce_random_pos ()=
  ((3 + Random.int (width-3)), (5 + Random.int (height-5)))

(**[is_dead snake enemies] checks whether [snake] hits walls or itself or
    [enemies]. *)
let is_dead snake enemies= 
  match snake with
  | [] -> false
  | [x; y] :: t -> let (hx, hy) = get_snake_head snake in 
    y = 4 || y = ter_hei-1 || x <= 1 || x >= width || List.mem [x;y] t ||
    List.mem (hx, hy) enemies|| List.mem (hx+1, hy) enemies
  | _ -> false

(**[new_state snake apple sl dir will_grow] creates a new state for
    the board every [sl+0.2] seconds. [dir] is the new direction of the snake. 
    If [will_grow], the snake grows by one segment in front.*)
let new_state snake apple (sl:float) dir (will_grow:bool)=
  (* sleepf(sl); *)
  set_cursor 1 row_top;
  let new_snake = if will_grow then snake |> snake_add_head dir
    else snake |> snake_add_head dir |> snake_remove_tail in
  let is_eat = check_eat apple new_snake in
  let new_apple = if is_eat then produce_random_pos()
    else apple in 
  (new_snake, new_apple, is_eat)


(**[new_state snake apple sl dir will_grow enemies] draws the board according to 
   new_st*)
let move snake apple (sl:float) dir (will_grow:bool) enemies=
  let (s, a, e) = new_state snake apple sl dir will_grow in
  let enemies' = if e then make_enemies s a true enemies else enemies in
  make_board width height s a enemies';(s,a,enemies')

(**[time_delay snake] is the speed depending on the length of [snake]. Large
    value means small speed. *)
let time_delay snake =
  let len = List.length snake in
  if len <= 10 then 5
  else if len <= 20 then 4
  else if len <= 30 then 3
  else if len <= 40 then 2
  else 1

(**[receive_input snake] is the direction depends on the butten being pressed. *)
let rec receive_input snake=
  let time = time_delay snake in
  let input = getachar time in
  match input with
  |'w' -> Up | 's' -> Down | 'a' -> Left | 'd' -> Right
  | _ -> receive_input snake;;

(**[is_opposite new_dir old_dir] checks whether the new direction is the 
    opposite of the old one.*)
let is_opposite new_dir old_dir = 
  match new_dir with
  |Up -> old_dir = Down
  |Down -> old_dir = Up
  |Left -> old_dir = Right
  |Right -> old_dir = Left

(** [game_over] prints a game over box over the last game board and 
    resets terminal*)
let game_over snake = 
  let pos = (ter_wid, ter_hei) in
  let box_w = 30 in 
  let box_h = 5 in 
  set_cursor (width/2 - box_w/2) (height/2 + (box_h/2 -3));
  print_endline (" " ^ draw_horiz_edge box_w);

  let rec vert xpos ypos h = 
    set_cursor xpos ypos;
    if h = 1 then (print_endline ("|" ^ (whitespace (box_w)) ^ "|"); 
                   print_endline) 
    else (print_endline ("|" ^ (whitespace (box_w)) ^ "|"); 
          vert xpos (ypos+1) (h-1))
  in
  vert (width/2 -box_w/2) (height/2 + (box_h/2 -2)) 5;

  set_cursor (width/2 -box_w/2) (height/2 + (box_h/2 + 3));
  print_endline (" " ^ draw_horiz_edge box_w);

  set_cursor (width/2 -4) (height/2 + (box_h/2 -1 ));
  print_string[red] ("GAME OVER ");
  set_cursor (width/2 -4) (height/2 + (box_h/2 + 1));
  print_string[blue] ("Score: " ^ string_of_int(List.length snake));
  set_cursor (fst pos) ((snd pos)+4);

  reset_terminal()

(** so check eat will check if snake is on apple i guess, and then eat will 
    create a new apple, move snake onto tile where apple was, and add 2 to grow. 
*)

(** [play_game ()] updates the canvas after each snake movement. *)
let play_game () =
  (* Random.init 10; *)
  let snake = [[width/2; height/2]] in
  let apple = produce_random_pos () in
  (* print_endline ((string_of_int (fst terminal_size)) ^"  "^ 
     (string_of_int (snd terminal_size))); *)
  make_board width height snake apple [];

  (* receives the user input and moves the snake*)
  (*[grow] is the number of iterations the snake should grow, incremented (by 2) 
    by eating an apple. decreases by one each turn the snake grows.*)
  let rec play n_snake n_apple old_dir (grow:int) enemies= 
    let will_grow = grow > 0 in
    (try
       (let input = receive_input snake in
        if is_opposite input old_dir then
          failwith "maintain the old direction" else (* will be catched*)
          let (new_snake, new_apple, enemies') = 
            move n_snake n_apple 0.1 input will_grow enemies in 
          let new_grow = (if check_eat n_apple new_snake then 2 else 0) + 
                         (if grow>0 then grow-1 else grow) in
          if is_dead new_snake enemies then game_over new_snake 
          else play new_snake new_apple input new_grow enemies')
     with
     |exp -> (let input = old_dir in 
              let (new_snake, new_apple, enemies') = 
                move n_snake n_apple 0.1 input will_grow enemies in 
              let new_grow = (if check_eat n_apple new_snake then 2 else 0) + 
                             (if grow>0 then grow-1 else grow) in
              if is_dead new_snake enemies then game_over new_snake
              else play new_snake new_apple input new_grow enemies'))
  in 
  play snake apple Left 0 (make_enemies snake apple true [])