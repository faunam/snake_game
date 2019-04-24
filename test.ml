open OUnit2

open State
open Display
open Enemies

(** tests for state.ml *)

(** [make_get_snake_head_test name snake expected_output] constructs an OUnit 
    test named [name] that asserts the equality of [expected_output] with 
    [get_snake_head snake]. *)
let make_get_snake_head_test 
    (name : string)
    (snake : 'a list list)
    (expected_output : 'a * 'a) : test =
  name >:: (fun _ ->
    assert_equal expected_output (get_snake_head snake))

(** [make_check_eat_test name apple apple_power snake expected_output] 
    constructs an OUnit test named [name] that asserts the equality of 
    [expected_output] with [check_eat apple apple_power snake]. *)
let make_check_eat_test
    (name : string)
    (apple : int * int)
    (apple_power : int)
    (snake : int list list)
    (expected_output : bool) : test =
  name >:: (fun _ ->
      assert_equal expected_output (check_eat apple apple_power snake))

(** [make_snake_add_head_test name dir snake expected_output] constructs an 
    OUnit test named [name] that asserts the equality of [expected_output] with
    [snake_add_head dir snake]. *)
let make_snake_add_head_test 
    (name : string)
    (dir : direction)
    (snake : int list list)
    (expected_output : int list list) : test =
  name >:: (fun _ -> 
      assert_equal expected_output (snake_add_head dir snake))

(** [make_snake_remove_tail_test name snake expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output] with 
    [snake_remove_tail snake]. *)
let make_snake_remove_tail_test
    (name : string)
    (snake : int list list)
    (expected_output : int list list) : test =
  name >:: (fun _ ->
      assert_equal expected_output (snake_remove_tail snake))

(** [make_is_dead_test name snake enemies expected_output] constructs an OUnit 
    test named [name] that asserts the equality of [expected_output] with 
    [is_dead snake enemies]. *)
let make_is_dead_test
    (name : string)
    (snake : int list list)
    (enemies: (int*int) list)
    (expected_output : bool) : test =
  name >:: (fun _ ->
      assert_equal expected_output (is_dead snake enemies))

(** [make_new_state_test name snake apple apple_power enemies sl dir will_grow]
    constructs an OUnit test named [name] that asserts the equality of 
    [expected_output] with [new_state snake apple apple_power enemies sl dir 
    will_grow]. *)
let make_new_state_test 
    (name : string)
    (snake : int list list)
    (apple : int * int)
    (apple_power : int)
    (enemies : (int * int) list)
    (sl : float)
    (dir : direction)
    (will_grow : bool)
    (expected_output : (int list list * (int * int) * int * bool)) : test =
  name >:: (fun _ ->
      assert_equal expected_output 
        (new_state snake apple apple_power enemies sl dir will_grow))

(** [make_is_opposite_test name new_dir old_dir expected_output] constructs an 
    OUnit test named [name] that asserts the equality of [expected_output] with
    [is_opposite new_dir old_dir]. *)
let make_is_opposite_test
    (name : string)
    (new_dir : direction)
    (old_dir : direction)
    (expected_output : bool) : test =
  name >:: (fun _ ->
      assert_equal expected_output (is_opposite new_dir old_dir))

(** [state_tests] is a list of OUnit tests for the functions in state.ml using
    the above functions. *)
let state_tests = [
(** tests for [get_snake_head snake]. *)
  make_get_snake_head_test "snake size 1" [[1;1]] (1,1);
  make_get_snake_head_test "snake size 2" [[1;1];[2;1]] (1,1);
  make_get_snake_head_test "snake size 3" [[3;3];[3;4];[3;5]] (3,3);

(** tests for [check_eat apple apple_power snake]. *)
  make_check_eat_test "check_eat true" (1,1) 4 [[1;1];[2;2]] true;
  make_check_eat_test "check_eat false" (3,3) 4 [[1;1];[2;2]] false;
  make_check_eat_test "check_eat false" (2,2) 4 [[1;1];[2;2]] false;
  make_check_eat_test "check_eat true" (1,1) 4 [[1;1]] true;
  make_check_eat_test "check_eat false" (1,2) 4 [[1;3]; [1;4]] false;

(** tests for [snake_add_head dir snake]. *)
  make_snake_add_head_test "snake_add_head up" Up [[2;2]] [[2;1];[2;2]];
  make_snake_add_head_test "snake_add_head down" Down [[2;2]] [[2;3];[2;2]];
  make_snake_add_head_test "snake_add_head left" Left [[3;3]] [[1;3];[3;3]];
  make_snake_add_head_test "snake_add_head right" Right [[3;3]] [[5;3];[3;3]];

(** tests for [snake_remove_tail snake]. *)
  make_snake_remove_tail_test "snake_remove_tail 0"[] [];
  make_snake_remove_tail_test "snake_remove_tail 1" [[1;1]] [];
  make_snake_remove_tail_test "snake_remove_tail 2" [[1;1];[2;2]] [[1;1]];
  make_snake_remove_tail_test "snake_remove_tail 5" 
    [[1;1];[2;2];[3;3];[4;4];[5;5]] [[1;1];[2;2];[3;3];[4;4]];

(** tests for [is_dead snake enemies]. *)
  make_is_dead_test "is dead top edge" [[5;4];[5;5];[5;6]] [(10,16)] true;
  make_is_dead_test "is dead left edge" [[1;5];[2;5];[2;4]] [(10,16)] true;
  make_is_dead_test "is dead right edge" [[58;6];[57;6];[56;6]] [(10,16)] true;
  make_is_dead_test "is dead self hit" 
    [[5;12];[5;13];[5;14];[4;14];[3;14];[3;13];[3;12];[4;12];[5;12];[6;12]]  
    [(10,16)] true;
  make_is_dead_test "is dead not dead" [[5;7];[5;6];[5;5]] [(10,16)] false;
  make_is_dead_test "is dead bottom edge" [[5;25]] [(10,16)] true;
  make_is_dead_test "is dead hit enemy" [[10;16]] [(10,16)] true;

(** tests for [new_state snake apple apple_power enemies sl dir will_grow]. *)

(** tests for [time_delay snake]. *)

(** tests for [is_opposite new_dir old_dir]. *)
  make_is_opposite_test "is_opposite up down" Up Down true;
  make_is_opposite_test "is_opposite up left" Up Left false;
  make_is_opposite_test "is_opposite up right" Up Right false;
  make_is_opposite_test "is_opposite left right" Left Right true;
  make_is_opposite_test "is_opposite right left" Right Left true;
  make_is_opposite_test "is_opposite down up" Down Up true;
  make_is_opposite_test "is_opposite right up"  Right Up false;
]


(** tests for display.ml *)

(** [make_whitespace_test name num expected_output] construts an OUnit test 
    named [name] that asserts the equality of [expected_output] with 
    [whitespace num]. *)
let make_whitespace_test 
    (name :  string)
    (num : int)
    (expected_output : string) : test =
  name >:: (fun _ -> 
      assert_equal expected_output (whitespace num))

(** [make_get_snake_seg_test name snake i expected_output] constructs an OUnit 
    test named [name] that asserts the equality of [expected_output] with
    [get_snake_seg snake i]. *)
let make_get_snake_seg_test
    (name : string)
    (snake : 'a list)
    (i : int)
    (expected_output : 'a) : test =
  name >:: (fun  _ -> 
      assert_equal expected_output (get_snake_seg snake i))

(** [make_get_seg_ycorr_test name seg expected_output] constructs an OUnit test
    named [name] that asserts the equality of [expected_output] with 
    [get_seg_ycorr seg]. *)
let make_get_seg_ycorr_test
    (name :  string)
    (seg : 'a list)
    (expected_output : 'a) : test =
  name  >:: (fun  _ ->
      assert_equal expected_output (get_seg_ycorr seg))

(** [make_get_seg_xcorr_test name seg expected_output] constructs an OUnit test
    named [name] that asserts the equality of [expected_output] with 
    [get_seg_xcorr seg]. *)
let make_get_seg_xcorr_test
    (name : string)
    (seg: 'a list)
    (expected_output : 'a) : test =
  name >:: (fun _ ->
      assert_equal expected_output (get_seg_xcorr seg))

(** [display_tests is a list of OUnit tests for the functions in display.ml 
    using the above functions. *)
let display_tests = [
  (** tests for [whitespace num]. *)
  make_whitespace_test "whitespace 0" 0 "";
  make_whitespace_test "whitespace 1" 1 " ";
  make_whitespace_test "whitespace 2" 2 "  ";
  make_whitespace_test "whitespace 7" 7 "       ";

  (** tests for [get_snake_seg snake i]. *)
  make_get_snake_seg_test "get_snake_seg 0" [[1;1]] 0 [1;1];
  make_get_snake_seg_test "get_snake_seg 1" [[1;1];[2;2]] 1 [2;2];
  make_get_snake_seg_test "get_snake_seg 0" [[1;1];[2;2]] 0 [1;1];
  make_get_snake_seg_test "get_snake_seg 3" [[1;1];[2;2];[3;3];[4;4];[5;5]] 3 
    [4;4];

  (** tests for [get_seg_ycorr seg]. *)
  make_get_seg_ycorr_test "get_seg_ycorr 2" [1;2] 2;
  make_get_seg_ycorr_test "get_seg_ycorr 45" [5;45] 45;
  make_get_seg_ycorr_test "get_seg_ycorr 50003" [5;50003] 50003;

  (** tests for [get_seg_xcorr seg]. *)
  make_get_seg_xcorr_test "get_seg_xcorr 1" [1;2] 1;
  make_get_seg_xcorr_test "get_seg_xcorr 78" [78;23] 78;
  make_get_seg_xcorr_test "get_seg_xcorr 12345" [12345; 203] 12345;

  (** tests for [update_h_score old_sc new_sc]. *)

]


(** tests for enemies.ml *)

(** [make_check_conflicts name snake apple apple_power enemies expected_output]
    constructs an OUnit test named [name] that asserts the equality of 
    [expected_output] with [check_conflicts snake apple apple_power enemies]. *)
let make_check_conflicts
    (name : string)
    (snake : 'a list list)
    (apple : 'a * 'a)
    (apple_power : int)
    (enemies : ('a * 'a) list)
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
    assert_equal expected_output 
      (check_conflicts snake apple apple_power enemies))

(** [enemies_tests] is a list of OUnit tests for the functions in enemies.ml 
    using the above functions. *)
let enemies_tests = [
  (** tests for [apple_extent apple power]. *)

  (** tests for [check_conflicts snake apple apple_power enemies]. *)
  make_check_conflicts "no conflict" [[1;1]] (2,2) 4 [(3,3)] false;
  make_check_conflicts "conflict with snake" [[3;3]] (2,2) 4 [(3,3)] true;
  make_check_conflicts "conflict with apple" [[1;1]] (3,3)4  [(3,3)] true;

  (** tests for [check_apple_conflicts snake enemies apple_pos apple_power]. *)

]

let tests = 
  "test suite  for A7" >::: List.flatten [
    state_tests;
    display_tests;
    enemies_tests
  ]

let _ = run_test_tt_main tests

