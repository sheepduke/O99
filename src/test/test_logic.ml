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

  ("table", `Quick, fun () ->
      check (list (pair (list (pair string bool)) bool))
        "[\"a\"; \"b\"] (And(Var \"a\", Or(Var \"a\", Var \"b\")))"
        [([("a", true); ("b", true)], true);
         ([("a", true); ("b", false)], true);
         ([("a", false); ("b", true)], false);
         ([("a", false); ("b", false)], false)]
        (table ["a"; "b"] (And(Var "a", Or(Var "a", Var "b"))));

      let a = Var "a" and b = Var "b" and c = Var "c" in
      check (list (pair (list (pair string bool)) bool))
        "(Or(And(a, Or(b,c)), Or(And(a,b), And(a,c))))"
        [([("a", true); ("b", true); ("c", true)], true);
         ([("a", true); ("b", true); ("c", false)], true);
         ([("a", true); ("b", false); ("c", true)], true);
         ([("a", true); ("b", false); ("c", false)], false);
         ([("a", false); ("b", true); ("c", true)], false);
         ([("a", false); ("b", true); ("c", false)], false);
         ([("a", false); ("b", false); ("c", true)], false);
         ([("a", false); ("b", false); ("c", false)], false)]
        (table ["a"; "b"; "c"] (Or(And(a, Or(b,c)), Or(And(a,b), And(a,c))))));

  ("gray", `Quick, fun () ->
      check (list string) "1" ["0"; "1"] (gray 1);
      check (list string) "2" ["00"; "01"; "11"; "10"] (gray 2);
      check (list string) "3"
        ["000"; "001"; "011"; "010"; "110"; "111"; "101"; "100"]
        (gray 3));
]
