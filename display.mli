(**[reset_terminal ()] resets the terminal to the state before the game begins. *)
val reset_terminal : unit -> unit

(**[get_snake_seg snake i] gets the [i]th segment of [snake]. *)
val get_snake_seg : int list list -> int -> int list

(**[get_seg_ycorr seg] gets the y corrdinate of the snake segment [seg]. *)
val get_seg_ycorr : int list -> int

(**[get_seg_xcorr seg] gets the leftmost x corrdinate of the snake segment 
    [seg]. *)
val get_seg_xcorr : int list -> int

val make_apple : int list list -> (int * int) list -> (int * int) * int

(**[make_board w h snake apple] draws the canvas with [snake] and [apple] 
    inside. [w] and [h] are the width and height of the canvas. *)
val make_board : int -> int -> int list list -> int * int -> int 
  -> (int * int) list -> unit

(*only have local high score if theres a way to lose points but keep playing*)
(**[update_h_score] holds the value of the high score for this session of the game. 
   It updates that value if the current score is greater.*)
val update_h_score : int -> int -> int

(** [game_over] prints a game over box over the last game board and 
    resets terminal*)
val game_over : int list list -> int -> unit