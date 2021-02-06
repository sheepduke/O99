open Alcotest

let test_suites: unit test list = [
  "List", Test_list.tests;
  "Arithmetic", Test_arithmetic.tests;
  "Logic", Test_logic.tests;
]

let () = run "All" test_suites
