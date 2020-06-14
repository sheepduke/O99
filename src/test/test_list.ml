open O99.List
open Alcotest

let option_int_2 = (option (pair int int))

let tests = [
  ("last []", `Quick, fun () ->
      check (option int) "" None (last []: int option));
  ("last [1]", `Quick, fun () ->
      check (option int) "" (Some 1) (last [1]));
  ("last [1, 2]", `Quick, fun () ->
      check (option int) "" (Some 2) (last [1; 2]));

  ("last_two []", `Quick, fun () ->
      check option_int_2 "" None (last_two []: (int * int) option));
  ("last_two [1]", `Quick, fun () ->
      check option_int_2 "" None (last_two [1]));
  ("last_two [1, 2]", `Quick, fun () ->
      check option_int_2 "" (Some (1, 2)) (last_two [1; 2]));
  ("last_two [1, 2, 3]", `Quick, fun () ->
      check option_int_2 "" (Some (2, 3)) (last_two [1; 2; 3]));

  ("at 0 []", `Quick, fun () ->
      check (option int) "" None (at 0 []: int option));
  ("at 0 [1]", `Quick, fun () ->
      check (option int) "" None (at 0 [1]));
  ("at 1 []", `Quick, fun () ->
      check (option int) "" None (at 1 []: int option));
  ("at 1 [1]", `Quick, fun () -> check (option int) "" (Some 1) (at 1 [1]));
  ("at 2 [1]", `Quick, fun () -> check (option int) "" None (at 2 [1]));
  ("at 2 [1, 2]", `Quick, fun () ->
      check (option int) "" (Some 2) (at 2 [1; 2]));

  ("length []", `Quick, fun () -> check int "" 0 (length []));
  ("length [1]", `Quick, fun () -> check int "" 1 (length [1]));
  ("length [1; 2]", `Quick, fun () -> check int "" 2 (length [1; 2]));

  ("rev []", `Quick, fun () -> check (list int) "" [] (rev []));
  ("rev [1]", `Quick, fun () -> check (list int) "" [1] (rev [1]));
  ("rev [1; 2]", `Quick, fun () -> check (list int) "" [2; 1] (rev [1; 2]));

  ("is_palidrome []", `Quick, fun () -> check bool "" true (is_palidrome []));
  ("is_palidrome [1]", `Quick, fun () -> check bool "" true (is_palidrome [1]));
  ("is_palidrome [1; 2; 1]", `Quick, fun () ->
      check bool "" true (is_palidrome [1; 2; 1]));
  ("is_palidrome [1; 2]", `Quick, fun () ->
      check bool "" false (is_palidrome [1; 2]));

  ("flatten []", `Quick, fun () -> check (list int) "" [] (flatten []));
  ("flatten [One 'a']", `Quick, fun () ->
      check (list char) "" ['a'] (flatten [One 'a']));
  ("flatten [One 'a'; Many[One 'b'; Many[One 'c'; One 'd']; One 'e']]", `Quick,
   fun () -> check (list char) "" ['a'; 'b'; 'c'; 'd'; 'e']
       (flatten [One 'a'; Many[One 'b'; Many[One 'c'; One 'd']; One 'e']]));

  ("compress []", `Quick, fun () -> check (list int) "" [] (compress []));
  ("compress [1]", `Quick, fun () -> check (list int) "" [1] (compress[1]));
  ("compress [1;1;2;2;3]", `Quick, fun () ->
      check (list int) "" [1; 2; 3] (compress [1; 1; 2; 2; 3]));

  ("pack []", `Quick, fun () -> check (list (list int)) "" [] (pack []));
  ("pack [1]", `Quick, fun () -> check (list (list int)) "" [[1]] (pack [1]));
  ("pack [1; 1; 2]", `Quick, fun () ->
      check (list (list int)) "" [[1; 1]; [2]] (pack [1; 1; 2]));
  ("pack [1; 1; 1; 2; 3; 3; 4; 1; 1]", `Quick, fun () ->
      check (list (list int)) "" [[1; 1; 1]; [2]; [3; 3]; [4]; [1; 1]]
        (pack [1; 1; 1; 2; 3; 3; 4; 1; 1]));

  ("encode []", `Quick, fun () -> check (list (pair int int)) "" [] (encode []));
  ("encode [1]", `Quick, fun () -> check (list (pair int int)) ""
       [(1, 1)] (encode [1]));
  ("encode [1; 1; 2]", `Quick, fun () -> check (list (pair int int)) ""
       [(2, 1); (1, 2)] (encode [1; 1; 2]));
  ("encode [1; 1; 1; 2; 2; 3; 4; 4; 1]", `Quick, fun () ->
      check (list (pair int int)) "" [(3, 1); (2, 2); (1, 3); (2, 4); (1, 1)]
        (encode [1; 1; 1; 2; 2; 3; 4; 4; 1]));

  (* TODO implement test for encode2 *)
]
