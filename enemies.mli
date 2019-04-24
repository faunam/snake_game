(** Width of the canvas. *)
val width : int

(** Height of the canvas. *)
val height : int

(** Height of the terminal. *)
val ter_hei : int

(** Width of the terminal. *)
val ter_wid : int

(** y cordinate of the top line of the canvas.*)
val row_top : int

(** [produce_random_pos] produces a random position inside the canvas. *)
val produce_random_pos: unit -> int * int

(** [check_power_apple_conflicts snake apple enemies power_apple_pos] checks 
    whether the [snake] head or [apple] or [enemies] overlaps with the
    [power_apple_pos].*)
val check_apple_conflicts : int list list -> (int * int) list -> int * int -> 
  int -> bool

val apple_extent : int * int -> int -> (int * int) list

(**[make_enemies snake apple is_hor enemies] are positions of all enemies. *)
val make_enemies: int list list -> int * int -> int -> bool -> (int * int) list
  -> (int * int) list