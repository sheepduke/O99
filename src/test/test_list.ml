open O99.List
open Alcotest

let type_last = (option int)
let type_last_two = (option (pair int int))
let type_at = (option int)

let tests = [
  ("last []", `Quick, fun () ->
      check type_last "Returns None" None (last []: int option));
  ("last [1]", `Quick, fun () ->
      check type_last "Returns Some 1" (Some 1) (last [1]));
  ("last [1, 2]", `Quick, fun () ->
      check type_last "Returns Some 2" (Some 2) (last [1; 2]));

  ("last_two []", `Quick, fun () ->
      check type_last_two "Returns None" None (last_two []: (int * int) option));
  ("last_two [1]", `Quick, fun () ->
      check type_last_two "Returns None" None (last_two [1]));
  ("last_two [1, 2]", `Quick, fun () ->
      check type_last_two "Returns Some 1 2" (Some (1, 2)) (last_two [1; 2]));
  ("last_two [1, 2, 3]", `Quick, fun () ->
      check type_last_two "Returns Some 1 2 3" (Some (2, 3)) (last_two [1; 2; 3]));

  ("at 0 []", `Quick, fun () ->
      check type_at "Returns None" None (at 0 []: int option));
  ("at 0 [1]", `Quick, fun () ->
      check type_at "Returns None" None (at 0 [1]));
  ("at 1 []", `Quick, fun () ->
      check type_at "Returns None" None (at 1 []: int option));
  ("at 1 [1]", `Quick, fun () ->
      check type_at "Returns Some 1" (Some 1) (at 1 [1]));
  ("at 2 [1]", `Quick, fun () ->
      check type_at "Returns None" None (at 2 [1]));
  ("at 2 [1, 2]", `Quick, fun () ->
      check type_at "Returns Some 2" (Some 2) (at 2 [1; 2]));
]
