open Display
open Enemies
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

open ANSITerminal
open Unix

(** Snake movement direction*)
type direction =
  |Up
  |Down
  |Left
  |Right

(**[get_snake_head snake] gets the snake head in tuple format. *)
let get_snake_head snake =
  let head = List.hd snake in
  (List.hd head, List.nth head 1)

(**[check_eat apple snake] checks whether [snake] can eat the [apple]. *)
let check_eat apple apple_power snake =
  let head = get_snake_seg snake 0 in
  let head_x = get_seg_xcorr head in
  let head_y = get_seg_ycorr head in
  let apple_extent = apple_extent apple apple_power in
  let rec check_apple_extent apple_extent = 
    match apple_extent with 
    | [] -> false 
    | h::t ->
      let apple_x = fst h in
      let apple_y = snd h in
      (apple_x == head_x &&  apple_y == head_y ||
       (apple_x == (head_x+1) && apple_y == head_y)) || check_apple_extent t in

  check_apple_extent apple_extent

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
let new_state snake apple apple_power enemies (sl:float) dir (will_grow:bool)=
  (* sleepf(sl); *)
  set_cursor 1 row_top;
  let new_snake = if will_grow then snake |> snake_add_head dir
    else snake |> snake_add_head dir |> snake_remove_tail in
  let is_eat_apple = check_eat apple apple_power new_snake in
  let apple_seed = make_apple new_snake enemies in 
  let new_apple = if is_eat_apple then fst apple_seed else apple in 
  let new_power = if is_eat_apple then snd apple_seed else if apple_power > 4 
    then apple_power - 1 else apple_power in 
  (new_snake, new_apple, new_power, is_eat_apple)

(**[new_state snake apple sl dir will_grow enemies] draws the board according to 
   new_st*)
let move snake apple apple_power (sl:float) dir (will_grow:bool) enemies=
  let (s, a, p, e) = new_state snake apple apple_power enemies sl dir will_grow in
  let enemies' = if e then make_enemies s a p true enemies else enemies in
  make_board width height s a p enemies';(s,a,p,enemies')

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

(** so check eat will check if snake is on apple i guess, and then eat will 
    create a new apple, move snake onto tile where apple was, and add 2 to grow. 
*)

(** [play_game ()] updates the canvas after each snake movement. *)
let play_game () =
  (* Random.init 10; *)
  let snake = [[width/2; height/2]] in
  let apple = produce_random_pos () in
  let apple_power = 4 + if Random.int 6 == 1 then 9 else 0 in 
  (* print_endline ((string_of_int (fst terminal_size)) ^"  "^ 
     (string_of_int (snd terminal_size))); *)
  make_board width height snake apple apple_power [];


  (*only update board if input is nothing, and update it with last input. 
    amd keep track of how long its been since last input - store time of last input
    and exectute function at time of next tick. -> change c_vtime to be the remainng time*)
  (* receives the user input and moves the snake*)
  (*[grow] is the number of iterations the snake should grow, incremented (by 2) 
    by eating an apple. decreases by one each turn the snake grows.*)
  let rec play n_snake n_apple (apple_power:int) old_dir (grow:int) enemies h_score= 
    let will_grow = grow > 0 in
    (try
       (let input = receive_input n_snake in
        if is_opposite input old_dir then
          failwith "maintain the old direction" else (* will be catched*)
          let (new_snake, new_apple, new_apple_power, enemies') = 
            move n_snake n_apple apple_power 0.1 input will_grow enemies in 
          let new_grow = (if check_eat n_apple apple_power new_snake then apple_power/2 else 0) + 
                         (if grow>0 then grow-1 else grow) in
          let new_h_sc = update_h_score h_score (List.length new_snake) in 
          if is_dead new_snake enemies then game_over new_snake new_h_sc
          else play new_snake new_apple new_apple_power input new_grow enemies' 
              new_h_sc)
     with
     |exp -> (let input = old_dir in 
              let (new_snake, new_apple, new_apple_power, enemies') = 
                move n_snake n_apple apple_power 0.1 input will_grow enemies in 
              let new_grow = (if check_eat n_apple apple_power new_snake then apple_power/2 else 0) + 
                             (if grow>0 then grow-1 else grow) in
              let new_h_sc = update_h_score h_score (List.length new_snake) in 
              if is_dead new_snake enemies then game_over new_snake new_h_sc
              else play new_snake new_apple new_apple_power input new_grow enemies' 
                  new_h_sc))
  in 
  play snake apple apple_power Left 0 
    (make_enemies snake apple 4 true []) 4