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

let main () = 
  Display.reset_terminal();
  resize ter_wid ter_hei;
  ANSITerminal.(print_string[red] "\n\ Welcome to Snake! 
  Use WASD to change direction. 
  Eat apples (red o's) to grow. 
  Eat power apples (purples O's) before they shrink to grow faster. 
  The longer the snake, the faster it moves. \n \n Press enter to start. \n");
  print_string[red] "> ";
  match read_line () with
  | exception _ -> ()
  | x -> State.play_game ()

let () = main ()