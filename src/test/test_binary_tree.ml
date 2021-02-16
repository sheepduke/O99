open O99.Binary_tree
open Alcotest

let testable_char_binary_tree=
  let pp_char_binary_tree ppf tree =
    O99.Binary_tree.pp (fun ppf value -> Format.fprintf ppf "%c" value) ppf tree
  in
  let equal_char_binary_tree left right =
    equal_binary_tree (fun a b -> Char.equal a b) left right
  in
  testable pp_char_binary_tree equal_char_binary_tree

let compare_char_binary_tree left right =
  compare_binary_tree (fun a b -> Char.compare a b) left right

let tests = [
  ("cbal_tree", `Quick, fun () ->
      check (list testable_char_binary_tree)
        "4"
        [Node ('x', Node ('x', Empty, Empty),
               Node ('x', Empty, Node ('x', Empty, Empty)));
         Node ('x', Node ('x', Empty, Empty),
               Node ('x', Node ('x', Empty, Empty), Empty));
         Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
               Node ('x', Empty, Empty));
         Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
               Node ('x', Empty, Empty))]
        (List.sort compare_char_binary_tree (cbal_tree 4));

      check int "40" 524288 (List.length (cbal_tree 40)));
]
