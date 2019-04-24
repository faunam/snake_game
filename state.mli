(** State creates the game state *)

(** the type of snakee movement direction*)
type direction =
  |Up
  |Down
  |Left
  |Right

(** [get_snake_head snake] gets the snake head in tuple format. *)
val get_snake_head : 'a list list -> 'a * 'a 

(**[check_eat apple snake] checks whether [snake] can eat the [apple]. *)
val check_eat : int * int -> int -> int list list -> bool

(** [snake_add_head dir snake] adds a new segment to the head of [snake] 
    following the direction [dir]. *)
val snake_add_head : direction -> int list list -> int list list

(** [snake_remove_tail snake] removes the last segment of [snake]. *)
val snake_remove_tail : int list list -> int list list

(** [is_dead snake cursor_pos] checks whether [snake] hits walls determined
    by [cursor_pos]  or itself. *)
val is_dead : int list list -> (int * int) list-> bool

(** [new_state snake apple apple_power enemies sl dir will_grow] creates a new
    state for the board every [sl+0.2] seconds. [dir] is the new direction of 
    the snake. If [will_grow], the snake grows by one segment in front. *)
val new_state : int list list -> int * int -> int -> (int * int) list -> float
     -> direction -> bool -> (int list list * (int * int) * int * bool)

(** [is_opposite new_dir old_dir] checks whether the new direction is the 
     opposite of the old one.*)
val is_opposite : direction -> direction -> bool

(** [play_game ()] updates the canvas after each snake movement. *)
val play_game : unit -> unit