open Offmain
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
  Offmain.reset_terminal();
  resize ter_wid ter_hei;
  ANSITerminal.(print_string[red] "\n\ Welcome to Snake! Use WASD to change 
    direction. Press enter to start \n");
  print_string[red] "> ";
  let cursor_pos = (ter_wid, ter_hei) in
  (* print_endline (string_of_int (fst cursor_pos) ^ "   " ^ 
     string_of_int (snd cursor_pos)); *)
  match read_line () with
  | exception _ -> ()
  | x -> Offmain.play_game cursor_pos

let () = main ()