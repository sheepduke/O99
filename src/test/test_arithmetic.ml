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
]
