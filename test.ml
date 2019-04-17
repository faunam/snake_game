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

let tests = 
 "test suite  for A6" >::: List.flatten [
   whitespace_tests;

  ]

let _ = run_test_tt_main tests

