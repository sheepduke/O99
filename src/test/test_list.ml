open O99.List
open Alcotest

let pp_rle_int ppf (rle : int rle) =
  match rle with
  | One value -> Fmt.pf ppf "One %d" value
  | Many (count, value) -> Fmt.pf ppf "Many (%d,%d)" count value

let equal_rle_int (x : 'a rle) (y : 'a rle) = x = y

let testable_rle_int = Alcotest.testable pp_rle_int equal_rle_int

let tests = [
  ("last []", `Quick, fun () ->
      check (option int) "" None (last []: int option));
  ("last [1]", `Quick, fun () ->
      check (option int) "" (Some 1) (last [1]));
  ("last [1, 2]", `Quick, fun () ->
      check (option int) "" (Some 2) (last [1; 2]));

  ("last_two []", `Quick, fun () ->
      check (option (pair int int)) "" None (last_two []: (int * int) option));
  ("last_two [1]", `Quick, fun () ->
      check (option (pair int int)) "" None (last_two [1]));
  ("last_two [1, 2]", `Quick, fun () ->
      check (option (pair int int)) "" (Some (1, 2)) (last_two [1; 2]));
  ("last_two [1, 2, 3]", `Quick, fun () ->
      check (option (pair int int)) "" (Some (2, 3)) (last_two [1; 2; 3]));

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

  ("encode2 []", `Quick, fun () -> check (list testable_rle_int) ""
       [] (encode2 []));
  ("encode2 [1]", `Quick, fun () -> check (list testable_rle_int) ""
       [(One 1)] (encode2 [1]));
  ("encode2 [1; 2; 2; 2; 3]", `Quick, fun () -> check (list testable_rle_int) ""
       [(One 1); (Many (3, 2)); (One (3))] (encode2 [1; 2; 2; 2; 3]));

  ("decode []", `Quick, fun () -> check (list int) ""
       [] (decode []));
  ("decode [One 1]", `Quick, fun () -> check (list int) ""
       [1] (decode [One 1]));
  ("decode [Many (3, 1); One 2; Many (2, 3); One 4]", `Quick, fun () ->
      check (list int) ""
        [1; 1; 1; 2; 3; 3; 4]
        (decode [Many (3, 1); One 2; Many (2, 3); One 4]));

  ("duplicate []", `Quick, fun () -> check (list int) "" [] (duplicate []));
  ("duplicate [1]", `Quick, fun () -> check (list int) "" [1; 1] (duplicate [1]));
  ("duplicate [1; 2]", `Quick, fun () -> check (list int) ""
       [1; 1; 2; 2] (duplicate [1; 2]));

  ("replicate [] 1", `Quick, fun () -> check (list int) "" [] (replicate [] 1));
  ("replicate [1] 2", `Quick, fun () -> check (list int) ""
       [1; 1] (replicate [1] 2));
  ("replicate [1; 2] 3", `Quick, fun () -> check (list int) ""
       [1; 1; 1; 2; 2; 2] (replicate [1; 2] 3));

  ("drop [] 1", `Quick, fun () -> check (list int) "" [] (drop [] 1));
  ("drop [1] 1", `Quick, fun () -> check (list int) "" [] (drop [1] 1));
  ("drop [1] 2", `Quick, fun () -> check (list int) "" [1] (drop [1] 2));
  ("drop [1; 2] 1", `Quick, fun () -> check (list int) "" [] (drop [1; 2] 1));
  ("drop [1; 2] 2", `Quick, fun () -> check (list int) "" [1] (drop [1; 2] 2));
  ("drop [1; 2; 3; 4] 2", `Quick, fun () -> check (list int) ""
       [1; 3] (drop [1; 2; 3; 4] 2));

  ("split [] 1", `Quick, fun () -> check (pair (list int) (list int)) ""
       ([], []) (split [] 1));
  ("split [1] 1", `Quick, fun () -> check (pair (list int) (list int)) ""
       ([1], []) (split [1] 1));
  ("split [1] 2", `Quick, fun () -> check (pair (list int) (list int)) ""
       ([1], []) (split [1] 2));
  ("split [1; 2] 1", `Quick, fun () -> check (pair (list int) (list int)) ""
       ([1], [2]) (split [1; 2] 1));
  ("split [1; 2; 3] 2", `Quick, fun () -> check (pair (list int) (list int)) ""
       ([1; 2], [3]) (split [1; 2; 3] 2));

  ("slice [] 0 1", `Quick, fun () -> check (list int) "" [] (slice [] 0 1));
  ("slice [1] 0 1", `Quick, fun () -> check (list int) "" [1] (slice [1] 0 1));
  ("slice [1; 2] 0 1", `Quick, fun () -> check (list int) ""
       [1; 2] (slice [1; 2] 0 1));
  ("slice [1; 2] 0 0", `Quick, fun () -> check (list int) ""
       [1] (slice [1; 2] 0 0));
  ("slice [1; 2; 3]", `Quick, fun () -> check (list int) ""
       [2; 3] (slice [1; 2; 3] 1 2));

  ("rotate [] 1", `Quick, fun () -> check (list int) ""
       [] (rotate [] 1));
  ("rotate [1] 1", `Quick, fun () -> check (list int) ""
       [1] (rotate [1] 1));
  ("rotate [1; 2] 1", `Quick, fun () -> check (list int) ""
       [2; 1] (rotate [1; 2] 1));

  ("remove_at 1 []", `Quick, fun () -> check (list int) ""
       [] (remove_at 1 []));
  ("remove_at 0 [1]", `Quick, fun () -> check (list int) ""
       [] (remove_at 0 [1]));
  ("remove_at 1 [1]", `Quick, fun () -> check (list int) ""
       [1] (remove_at 1 [1]));
  ("remove_at 0 [1; 2; 3; 4]", `Quick, fun () -> check (list int) ""
       [2; 3; 4] (remove_at 0 [1; 2; 3; 4]));
  ("remove_at 1 [1; 2; 3; 4]", `Quick, fun () -> check (list int) ""
       [1; 3; 4] (remove_at 1 [1; 2; 3; 4]));

  ("insert_at 1 1 []", `Quick, fun () -> check (list int) ""
       [1] (insert_at 1 1 []));
  ("insert_at 1 0 []", `Quick, fun () -> check (list int) ""
       [1] (insert_at 1 0 []));
  ("insert_at 0 0 [1; 2]", `Quick, fun () -> check (list int) ""
       [0; 1; 2] (insert_at 0 0 [1; 2]));
  ("insert_at 3 2 [1; 2]", `Quick, fun () -> check (list int) ""
       [1; 2; 3] (insert_at 3 2 [1; 2]));

  ("range 0 0", `Quick, fun () -> check (list int) "" [0] (range 0 0));
  ("range 4 9", `Quick, fun () -> check (list int) ""
       [4; 5; 6; 7; 8; 9] (range 4 9));
]
