open Alcotest

let test_suites: unit test list = [
  "List", Test_list.tests;
  "Arithmetic", Test_arithmetic.tests;
  "Logic", Test_logic.tests;
  "Binary Tree", Test_binary_tree.tests;
]

let () = run "All" test_suites
