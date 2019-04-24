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

(** [apple_extent apple power] produces the [apple] based on its [power]. The
    bigger the int [power], the bigger the apple. *)
val apple_extent : int * int -> int -> (int * int) list

(** [check_conflicts snake apple apple_power enemies] checks whether the [snake]
    head or [apple] overlaps with [enemies]. *)
val check_conflicts : int list list -> int * int -> int -> (int * int) list -> bool

(** [check_power_apple_conflicts snake apple enemies power_apple_pos] checks 
    whether the [snake] head or [apple] or [enemies] overlaps with the
    [power_apple_pos].*)
val check_apple_conflicts : int list list -> (int * int) list -> int * int -> 
  int -> bool

(**[make_enemies snake apple is_hor enemies] are positions of all enemies. *)
val make_enemies: int list list -> int * int -> int -> bool -> (int * int) list
  -> (int * int) list