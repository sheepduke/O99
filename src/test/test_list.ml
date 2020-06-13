open O99.List
open Alcotest

let option_int = (option int)
let option_int_2 = (option (pair int int))

let tests = [
  ("last []", `Quick, fun () ->
      check option_int "Returns None" None (last []: int option));
  ("last [1]", `Quick, fun () ->
      check option_int "Returns Some 1" (Some 1) (last [1]));
  ("last [1, 2]", `Quick, fun () ->
      check option_int "Returns Some 2" (Some 2) (last [1; 2]));

  ("last_two []", `Quick, fun () ->
      check option_int_2 "Returns None" None (last_two []: (int * int) option));
  ("last_two [1]", `Quick, fun () ->
      check option_int_2 "Returns None" None (last_two [1]));
  ("last_two [1, 2]", `Quick, fun () ->
      check option_int_2 "Returns Some 1 2" (Some (1, 2)) (last_two [1; 2]));
  ("last_two [1, 2, 3]", `Quick, fun () ->
      check option_int_2 "Returns Some 1 2 3" (Some (2, 3)) (last_two [1; 2; 3]));

  ("at 0 []", `Quick, fun () ->
      check option_int "Returns None" None (at 0 []: int option));
  ("at 0 [1]", `Quick, fun () ->
      check option_int "Returns None" None (at 0 [1]));
  ("at 1 []", `Quick, fun () ->
      check option_int "Returns None" None (at 1 []: int option));
  ("at 1 [1]", `Quick, fun () ->
      check option_int "Returns Some 1" (Some 1) (at 1 [1]));
  ("at 2 [1]", `Quick, fun () ->
      check option_int "Returns None" None (at 2 [1]));
  ("at 2 [1, 2]", `Quick, fun () ->
      check option_int "Returns Some 2" (Some 2) (at 2 [1; 2]));

  ("length []", `Quick, fun () -> check int "Returns 0" 0 (length []));
  ("length [1]", `Quick, fun () -> check int "Returns 1" 1 (length [1]));
  ("length [1; 2]", `Quick, fun () -> check int "Returns 2" 2 (length [1; 2]));

  ("rev []", `Quick, fun () ->
      check (list int) "Returns []" [] (rev []));
  ("rev [1]", `Quick, fun () ->
      check (list int) "Returns [1]" [1] (rev [1]));
  ("rev [1; 2]", `Quick, fun () ->
      check (list int) "Returns [2; 1]" [2; 1] (rev [1; 2]));

  ("is_palidrome []", `Quick, fun () ->
      check bool "Returns true" true (is_palidrome []));
  ("is_palidrome [1]", `Quick, fun () ->
      check bool "Returns true" true (is_palidrome [1]));
  ("is_palidrome [1; 2; 1]", `Quick, fun () ->
      check bool "Returns true" true (is_palidrome [1; 2; 1]));
  ("is_palidrome [1; 2]", `Quick, fun () ->
      check bool "Returns false" false (is_palidrome [1; 2]));

  ("flatten []", `Quick, fun () ->
      check (list int) "Returns []" [] (flatten []));
  ("flatten [One 'a']", `Quick, fun () ->
      check (list char) "Returns ['a']" ['a'] (flatten [One 'a']));
  ("flatten [One 'a'; Many[One 'b'; Many[One 'c'; One 'd']; One 'e']]", `Quick,
   fun () -> check (list char) "Returns ['a'; 'b'; 'c'; 'd'; 'e']"
       ['a'; 'b'; 'c'; 'd'; 'e']
       (flatten [One 'a'; Many[One 'b'; Many[One 'c'; One 'd']; One 'e']]));
]
