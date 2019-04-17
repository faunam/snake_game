open OUnit2

open Main

let make_whitespace_test 
  (name :  string)
  (num : int)
  (expected_output : string) : test =
  name >:: (fun _ -> 
    assert_equal expected_output (whitespace num))

let whitespace_tests = [
  make_whitespace_test "whitespace 0" 0 "";
  make_whitespace_test "whitespace 1" 1 " ";
  make_whitespace_test "whitespace 2" 2 "  ";
  make_whitespace_test "whitespace 7" 7 "       ";
]

let make_get_snake_seg_test
  (name : string)
  (snake : 'a list)
  (i : int)
  (expected_output : 'a) : test =
  name >:: (fun  _ -> 
    assert_equal expected_output (get_snake_seg snake i))

let get_snake_seg_tests = [
  make_get_snake_seg_test "get_snake_seg 0" [[1;1]] 0 [1;1];
  make_get_snake_seg_test "get_snake_seg 1" [[1;1];[2;2]] 1 [2;2];
  make_get_snake_seg_test "get_snake_seg 0" [[1;1];[2;2]] 0 [1;1];
  make_get_snake_seg_test "get_snake_seg 3" [[1;1];[2;2];[3;3];[4;4];[5;5]] 3 
    [4;4];
]

let make_get_seg_ycorr_test
  (name :  string)
  (seg : 'a list)
  (expected_output : 'a) : test =
  name  >:: (fun  _ ->
    assert_equal expected_output (get_seg_ycorr seg))

let get_seg_ycorr_tests = [
  make_get_seg_ycorr_test "get_seg_ycorr 2" [1;2] 2;
] 

let make_get_seg_xcorr_test
  (name : string)
  (seg: 'a list)
  (expected_output : 'a) : test =
  name >:: (fun _ ->
    assert_equal expected_output (get_seg_xcorr seg))

let get_seg_xcorr_tests = [
  make_get_seg_xcorr_test "get_seg_xcorr 1" [1;2] 1;
]

let make_check_eat_test
  (name : string)
  (apple : int * int)
  (snake : int list list)
  (expected_output : bool) : test =
  name >:: (fun _ ->
    assert_equal expected_output (check_eat apple snake))

let check_eat_tests = [
  make_check_eat_test "check_eat true" (1,1) [[1;1];[2;2]] true;
  make_check_eat_test "check_eat false" (3,3) [[1;1];[2;2]] false;
]

let make_snake_add_head_test 
  (name : string)
  (dir : direction)
  (snake : int list list)
  (expected_output : int list list) : test =
  name >:: (fun _ -> 
    assert_equal expected_output (snake_add_head dir snake))

let snake_add_head_tests = [
  make_snake_add_head_test "snake_add_head up" Up [[2;2]] [[2;1];[2;2]];
  make_snake_add_head_test "snake_add_head down" Down [[2;2]] [[2;3];[2;2]];
  make_snake_add_head_test "snake_add_head left" Left [[3;3]] [[1;3];[3;3]];
  make_snake_add_head_test "snake_add_head right" Right [[3;3]] [[5;3];[3;3]]
]

let tests = 
 "test suite  for A6" >::: List.flatten [
   whitespace_tests;
   get_snake_seg_tests;
   get_seg_ycorr_tests;
   get_seg_xcorr_tests;
   check_eat_tests;
   snake_add_head_tests;
  
  ]

let _ = run_test_tt_main tests

