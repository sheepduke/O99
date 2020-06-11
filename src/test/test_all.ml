open Alcotest

let test_suites: unit test list = [
  "List", Test_list.tests;
]

let () = run "All" test_suites
