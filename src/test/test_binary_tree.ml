open O99.Binary_tree
open Alcotest

let char_binary_tree=
  let pp_char_binary_tree ppf tree =
    O99.Binary_tree.pp (fun ppf value -> Format.fprintf ppf "%c" value) ppf tree
  in
  let equal_char_binary_tree left right =
    equal_binary_tree (fun a b -> Char.equal a b) left right
  in
  testable pp_char_binary_tree equal_char_binary_tree

let int_binary_tree=
  let pp_int_binary_tree ppf tree =
    O99.Binary_tree.pp (fun ppf value -> Format.fprintf ppf "%d" value) ppf tree
  in
  let equal_int_binary_tree left right =
    equal_binary_tree (fun a b -> Int.equal a b) left right
  in
  testable pp_int_binary_tree equal_int_binary_tree

let compare_char_binary_tree left right =
  compare_binary_tree (fun a b -> Char.compare a b) left right

let compare_int_binary_tree left right =
  compare_binary_tree (fun a b -> Int.compare a b) left right

let tests = [
  ("cbal_tree", `Quick, fun () ->
      check (list char_binary_tree)
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

  ("is_symmetric", `Quick, fun () ->
      check bool "Empty" true (is_symmetric Empty);
      check bool "Node (1, Node (2, Empty, Empty), Empty)" false
        (is_symmetric (Node (1, Node (2, Empty, Empty), Empty)));
      check bool "Node (1, Node (2, Empty, Empty), Node (3, Empty, Empty))"
        true
        (is_symmetric (Node (1, Node (2, Empty, Empty), Node (3, Empty, Empty)))));

  ("construct", `Quick, fun () ->
      check int_binary_tree "[]" Empty (construct []);
      check int_binary_tree "[1]" (Node (1, Empty, Empty)) (construct [1]);
      check int_binary_tree "[1; 2]"
        (Node (1, Empty, Node (2, Empty, Empty))) (construct [1; 2]);
      check int_binary_tree "[2; 1; 3]"
        (Node (2, Node (1, Empty, Empty), Node (3, Empty, Empty)))
        (construct [2; 1; 3]);
      check int_binary_tree "[3; 1; 2; 7; 5]"
        (Node (3, Node (1, Empty, Node (2, Empty, Empty)),
               Node (7, Node (5, Empty, Empty), Empty)))
        (construct [3; 1; 2; 7; 5]));

  ("sym_cbal_tree", `Quick, fun () ->
      check (list char_binary_tree) "5"
        [Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
               Node ('x', Node ('x', Empty, Empty), Empty));
         Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
               Node ('x', Empty, Node ('x', Empty, Empty)))]
        (List.sort compare_char_binary_tree (sym_cbal_trees 5));

      check int "57" 256 (List.length (sym_cbal_trees 57));

      check (list (pair int int)) "range 10 20"
        [(10, 0); (11, 4); (12, 0); (13, 4); (14, 0); (15, 1); (16, 0);
         (17, 8); (18, 0); (19, 16); (20, 0)]
        (List.map (fun n -> n, List.length(sym_cbal_trees n))
           (O99.List.range 10 20)));

  ("hbal_tree", `Quick, fun () ->
      check (list char_binary_tree) "3"
        [Node ('x', Node ('x', Empty, Empty),
               Node ('x', Empty, Node ('x', Empty, Empty)));
         Node ('x', Node ('x', Empty, Empty),
               Node ('x', Node ('x', Empty, Empty), Empty));
         Node ('x', Node ('x', Empty, Empty),
               Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)));
         Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
               Node ('x', Empty, Empty));
         Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
               Node ('x', Empty, Node ('x', Empty, Empty)));
         Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
               Node ('x', Node ('x', Empty, Empty), Empty));
         Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
               Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)));
         Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
               Node ('x', Empty, Empty));
         Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
               Node ('x', Empty, Node ('x', Empty, Empty)));
         Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
               Node ('x', Node ('x', Empty, Empty), Empty));
         Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
               Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)));
         Node ('x', Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)),
               Node ('x', Empty, Empty));
         Node ('x', Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)),
               Node ('x', Empty, Node ('x', Empty, Empty)));
         Node ('x', Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)),
               Node ('x', Node ('x', Empty, Empty), Empty));
         Node ('x', Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)),
               Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)))]
        (List.sort compare_char_binary_tree (hbal_tree 3)));
]
