open O99.Logic
open Alcotest

let triple a b c =
  let eq (a1, b1, c1) (a2, b2, c2) =
    Alcotest.equal a a1 a2
    && Alcotest.equal b b1 b2
    && Alcotest.equal c c1 c2
  in
  let pp =
    Fmt.(parens (
        using (fun (x, _, _) -> x) (box (Alcotest.pp a))
        ++ comma
        ++ using (fun (_, x, _) -> x) (box (Alcotest.pp b))
        ++ comma
        ++ using (fun (_, _, x) -> x) (box (Alcotest.pp c))
      ))
  in
  Alcotest.testable pp eq


let tests = [
  ("table2", `Quick, fun () ->
      check (list (triple bool bool bool))
        "(And(Var \"a\", Or(Var \"a\", Var \"b\")))"
        [(true, true, true); (true, false, true);
         (false, true, false); (false, false, false)]
        (table2 "a" "b" (And(Var "a", Or(Var "a", Var "b"))))
  );
]
