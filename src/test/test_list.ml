open O99.List
open Alcotest

let pp_rle_int ppf (rle : int rle) =
  match rle with
  | One value -> Fmt.pf ppf "One %d" value
  | Many (count, value) -> Fmt.pf ppf "Many (%d,%d)" count value

let equal_rle_int (x : 'a rle) (y : 'a rle) = x = y

let testable_rle_int = Alcotest.testable pp_rle_int equal_rle_int

let tests = [
  ("last", `Quick, fun () ->
      check (option int) "[]" None (last []: int option);
      check (option int) "[1]" (Some 1) (last [1]);
      check (option int) "[1, 2]" (Some 2) (last [1; 2]));

  ("last_two", `Quick, fun () ->
      check (option (pair int int)) "[]" None (last_two []: (int * int) option);
      check (option (pair int int)) "[1]" None (last_two [1]);
      check (option (pair int int)) "[1, 2]" (Some (1, 2)) (last_two [1; 2]);
      check (option (pair int int)) "[1, 2, 3]" (Some (2, 3)) (last_two [1; 2; 3]));

  ("at", `Quick, fun () ->
      check (option int) "0 []" None (at 0 []: int option);
      check (option int) "0 [1]" None (at 0 [1]);
      check (option int) "1 []" None (at 1 []: int option);
      check (option int) "1 [1]" (Some 1) (at 1 [1]);
      check (option int) "2 [1]" None (at 2 [1]);
      check (option int) "2 [1, 2]" (Some 2) (at 2 [1; 2]));

  ("length", `Quick, fun () ->
      check int "[]" 0 (length []);
      check int "[1]" 1 (length [1]);
      check int "[1; 2]" 2 (length [1; 2]));

  ("rev", `Quick, fun () ->
      check (list int) "[]" [] (rev []);
      check (list int) "[1]" [1] (rev [1]);
      check (list int) "[1; 2]" [2; 1] (rev [1; 2]));

  ("is_palidrome", `Quick, fun () ->
      check bool "[]" true (is_palidrome []);
      check bool "[1]" true (is_palidrome [1]);
      check bool "[1; 2; 1]" true (is_palidrome [1; 2; 1]);
      check bool "[1; 2]" false (is_palidrome [1; 2]));

  ("flatten", `Quick, fun () ->
      check (list int) "[]" [] (flatten []);
      check (list char) "[One 'a']" ['a'] (flatten [One 'a']);
      check (list char) "[One 'a'; Many[One 'b'; Many[One 'c'; One 'd']; One 'e']]"
        ['a'; 'b'; 'c'; 'd'; 'e']
        (flatten [One 'a'; Many[One 'b'; Many[One 'c'; One 'd']; One 'e']]));

  ("compress", `Quick, fun () ->
      check (list int) "[]" [] (compress []);
      check (list int) "[1]" [1] (compress[1]);
      check (list int) "[1;1;2;2;3]" [1; 2; 3] (compress [1; 1; 2; 2; 3]));

  ("pack", `Quick, fun () ->
      check (list (list int)) "[]" [] (pack []);
      check (list (list int)) "[1]" [[1]] (pack [1]);
      check (list (list int)) "[1; 1; 2]" [[1; 1]; [2]] (pack [1; 1; 2]);
      check (list (list int)) "[1; 1; 1; 2; 3; 3; 4; 1; 1]"
        [[1; 1; 1]; [2]; [3; 3]; [4]; [1; 1]]
        (pack [1; 1; 1; 2; 3; 3; 4; 1; 1]));

  ("encode", `Quick, fun () ->
      check (list (pair int int)) "[]" [] (encode []);
      check (list (pair int int)) "[1]" [(1, 1)] (encode [1]);
      check (list (pair int int)) "[1; 1; 2]" [(2, 1); (1, 2)] (encode [1; 1; 2]);
      check (list (pair int int)) "[1; 1; 1; 2; 2; 3; 4; 4; 1]"
        [(3, 1); (2, 2); (1, 3); (2, 4); (1, 1)]
        (encode [1; 1; 1; 2; 2; 3; 4; 4; 1]));

  ("encode2", `Quick, fun () ->
      check (list testable_rle_int) "[]" [] (encode2 []);
      check (list testable_rle_int) "[1]" [(One 1)] (encode2 [1]);
      check (list testable_rle_int) "[1; 2; 2; 2; 3]"
        [(One 1); (Many (3, 2)); (One (3))]
        (encode2 [1; 2; 2; 2; 3]));

  ("decode", `Quick, fun () ->
      check (list int) "[]" [] (decode []);
      check (list int) "[One 1]" [1] (decode [One 1]);
      check (list int) "[Many (3, 1); One 2; Many (2, 3); One 4]"
        [1; 1; 1; 2; 3; 3; 4]
        (decode [Many (3, 1); One 2; Many (2, 3); One 4]));

  ("duplicate", `Quick, fun () ->
      check (list int) "[]" [] (duplicate []);
      check (list int) "[1]" [1; 1] (duplicate [1]);
      check (list int) "[1; 2]" [1; 1; 2; 2] (duplicate [1; 2]));

  ("replicate", `Quick, fun () ->
      check (list int) "[] 1" [] (replicate [] 1);
      check (list int) "[1] 2" [1; 1] (replicate [1] 2);
      check (list int) "[1; 2] 3" [1; 1; 1; 2; 2; 2] (replicate [1; 2] 3));

  ("drop", `Quick, fun () ->
      check (list int) "[] 1" [] (drop [] 1);
      check (list int) "[1] 1" [] (drop [1] 1);
      check (list int) " [1] 2" [1] (drop [1] 2);
      check (list int) " [1; 2] 1" [] (drop [1; 2] 1);
      check (list int) "[1; 2] 2" [1] (drop [1; 2] 2);
      check (list int) "[1; 2; 3; 4] 2" [1; 3] (drop [1; 2; 3; 4] 2));

  ("split", `Quick, fun () ->
      check (pair (list int) (list int)) "[] 1" ([], []) (split [] 1);
      check (pair (list int) (list int)) "[1] 1" ([1], []) (split [1] 1);
      check (pair (list int) (list int)) "[1] 2" ([1], []) (split [1] 2);
      check (pair (list int) (list int)) "[1; 2] 1" ([1], [2]) (split [1; 2] 1);
      check (pair (list int) (list int)) "[1; 2; 3] 2"
        ([1; 2], [3]) (split [1; 2; 3] 2));

  ("slice", `Quick, fun () ->
      check (list int) "[] 0 1" [] (slice [] 0 1);
      check (list int) "[1] 0 1" [1] (slice [1] 0 1);
      check (list int) "[1; 2] 0 1" [1; 2] (slice [1; 2] 0 1);
      check (list int) "[1; 2] 0 0" [1] (slice [1; 2] 0 0);
      check (list int) "[1; 2; 3] 1 2" [2; 3] (slice [1; 2; 3] 1 2));

  ("rotate", `Quick, fun () ->
      check (list int) "[] 1" [] (rotate [] 1);
      check (list int) "[1] 1" [1] (rotate [1] 1);
      check (list int) "[1; 2] 1" [2; 1] (rotate [1; 2] 1));

  ("remove_at", `Quick, fun () ->
      check (list int) "1 []" [] (remove_at 1 []);
      check (list int) "0 [1]" [] (remove_at 0 [1]);
      check (list int) "1 [1]" [1] (remove_at 1 [1]);
      check (list int) "0 [1; 2; 3; 4]" [2; 3; 4] (remove_at 0 [1; 2; 3; 4]);
      check (list int) "1 [1; 2; 3; 4]" [1; 3; 4] (remove_at 1 [1; 2; 3; 4]));

  ("insert_at  []", `Quick, fun () ->
      check (list int) "1 1 []" [1] (insert_at 1 1 []);
      check (list int) "1 0 []" [1] (insert_at 1 0 []);
      check (list int) "0 0 [1; 2]" [0; 1; 2] (insert_at 0 0 [1; 2]);
      check (list int) "3 2 [1; 2]" [1; 2; 3] (insert_at 3 2 [1; 2]));

  ("range", `Quick, fun () ->
      check (list int) "0 0" [0] (range 0 0);
      check (list int) "4 9" [4; 5; 6; 7; 8; 9] (range 4 9));

  ("extract", `Quick, fun () ->
      check (list (list char)) "2 ['a';'b';'c';'d']"
        [['a'; 'b']; ['a'; 'c']; ['a'; 'd']; ['b'; 'c']; ['b'; 'd']; ['c'; 'd']]
        (extract 2 ['a';'b';'c';'d']);

      check (list (list int)) "2 [1; 2; 3; 4; 5]"
        [[1; 2]; [1; 3]; [1; 4]; [1; 5]; [2; 3]; [2; 4]; [2; 5];
         [3; 4]; [3; 5]; [4; 5]]
        (extract 2 [1; 2; 3; 4; 5]);

      check (list (list int)) "3 [1; 2; 3; 4]"
        [[1; 2; 3]; [1; 2; 4]; [1; 3; 4]; [2; 3; 4]]
        (extract 3 [1; 2; 3; 4]));
]
