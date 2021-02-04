open O99.Arithmetic
open Alcotest

let tests = [
  ("is_prime", `Quick, fun () ->
      check bool "2" true (is_prime 2);
      check bool "3" true (is_prime 3);
      check bool "4" false (is_prime 4);
      check bool "5" true (is_prime 5);
      check bool "6" false (is_prime 6);
      check bool "7" true (is_prime 7);
      check bool "8" false (is_prime 8);
      check bool "9" false (is_prime 9);
      check bool "10" false (is_prime 10);
      check bool "11" true (is_prime 11));

  ("gcd", `Quick, fun () ->
      check int "2 3" 1 (gcd 2 3);
      check int "2 4" 2 (gcd 2 4);
      check int "2 5" 1 (gcd 2 5);
      check int "6 8" 2 (gcd 6 8));

  ("coprime", `Quick, fun () ->
      check bool "13 27" true (coprime 13 27);
      check bool "2 4" false (coprime 2 4);
      check bool "20536 7826" false (coprime 20536 7826));

  ("phi", `Quick, fun () ->
      check int "10" 4 (phi 10);
      check int "13" 12 (phi 13));

  ("factors", `Quick, fun () ->
      check (list int) "2" [2] (factors 2);
      check (list int) "10" [2; 5] (factors 10);
      check (list int) "20" [2; 2; 5] (factors 20);
      check (list int) "315" [3; 3; 5; 7] (factors 315));

  ("factors2", `Quick, fun () ->
      check (list (pair int int)) "2" [(2, 1)] (factors2 2);
      check (list (pair int int)) "10" [(2, 1); (5, 1)] (factors2 10);
      check (list (pair int int)) "20" [(2, 2); (5, 1)] (factors2 20);
      check (list (pair int int)) "315" [(3, 2); (5, 1); (7, 1)] (factors2 315));

  ("phi_improved", `Quick, fun () ->
      check int "10" 4 (phi_improved 10);
      check int "13" 12 (phi_improved 13));

  ("all_primes", `Quick, fun () ->
      check (list int) "2 10" [2; 3; 5; 7] (all_primes 2 10);
      check int "2 7920" 1000 (Stdlib.List.length (all_primes 2 7920)));

  ("goldbach", `Quick, fun () ->
      check (pair int int) "10" (3, 7) (goldbach 10);
      check (pair int int) "20" (3, 17) (goldbach 20);
      check (pair int int) "28" (5, 23) (goldbach 28));

  ("goldbach_list", `Quick, fun () ->
      check (list (pair int (pair int int))) "9 20"
        [(10, (3, 7)); (12, (5, 7)); (14, (3, 11));
         (16, (3, 13)); (18, (5, 13)); (20, (3, 17))]
        (goldbach_list 9 20));
]
