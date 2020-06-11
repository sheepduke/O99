open O99.List
open Alcotest

let tests = [
  ("last []", `Quick, fun () ->
      check (option int) "Returns None" None (last ([]: int list)));
  ("last [1]", `Quick, fun () ->
      check (option int) "Returns Some 1" (Some 1) (last [1]));
  ("last [1, 2]", `Quick, fun () ->
      check (option int) "Returns Some 2" (Some 2) (last [1; 2]));
]
