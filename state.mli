(** State creates the game state *)

(** the type of snakee movement direction*)
type direction =
  |Up
  |Down
  |Left
  |Right

(**[reset_terminal ()] resets the terminal to the state before the game begins.
*)
val reset_terminal : unit -> unit

(** [play_game cursor_pos] updates the canvas after each snake movement. *)
val play_game : int * int -> unit

(** [whitespace num] is a string composed of only whitespace with [num]. *)
val whitespace : int -> string

(**[get_snake_seg snake i] gets the [i]th segment of [snake]. *)
val get_snake_seg : int list list -> int -> int list

(**[get_seg_ycorr seg] gets the y corrdinate of the snake segment [seg]. *)
val get_seg_ycorr : int list -> int 

(**[get_seg_xcorr seg] gets the leftmost x corrdinate of the snake segment 
    [seg]. *)
val get_seg_xcorr : int list -> int

(**[check_eat apple snake] checks whether [snake] can eat the [apple]. *)
val check_eat : int * int -> int list list -> bool

(** [snake_add_head dir snake] adds a new segment to the head of [snake] 
    following the direction [dir]. *)
val snake_add_head : direction -> int list list -> int list list

(** [snake_remove_tail snake] removes the last segment of [snake]. *)
val snake_remove_tail : int list list -> int list list

(** [is_opposite new_dir old_dir] checks whether the new direction is the 
     opposite of the old one.*)
val is_opposite : direction -> direction -> bool

(** [is_dead snake cursor_pos] checks whether [snake] hits walls determined
    by [cursor_pos]  or itself. *)
val is_dead : int list list -> int * int -> bool
