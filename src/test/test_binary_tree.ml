open O99.Binary_tree
open Alcotest

let show_char ppf value = Format.fprintf ppf "%c" value

let char_binary_tree =
  let pp ppf tree = O99.Binary_tree.pp show_char ppf tree in
  let equal left right =
    equal_binary_tree (fun a b -> Char.equal a b) left right
  in
  testable pp equal

let show_int ppf value = Format.fprintf ppf "%d" value

let int_binary_tree =
  let pp ppf tree = O99.Binary_tree.pp show_int ppf tree in
  let equal left right =
    equal_binary_tree (fun a b -> Int.equal a b) left right
  in
  testable pp equal

let compare_char_binary_tree left right =
  compare_binary_tree (fun a b -> Char.compare a b) left right

let compare_int_binary_tree left right =
  compare_binary_tree (fun a b -> Int.compare a b) left right

let show_char_int_int ppf (char_value, int_value_1, int_value_2) =
  Format.fprintf ppf "(%c, %d, %d)" char_value int_value_1 int_value_2

let char_int_int_binary_tree =
  let pp ppf tree = O99.Binary_tree.pp show_char_int_int ppf tree in
  let equal left right =
    equal_binary_tree (fun a b -> a = b) left right
  in
  testable pp equal

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

  ("count_leaves", `Quick, fun () ->
      let tree = Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
                       Node ('x', Node ('x', Empty, Empty), Empty))
      in
      check int (show_binary_tree show_char tree)
        2 (count_leaves tree));

  ("leaves", `Quick, fun () ->
      let tree = Node (1, Node (2, Empty, Node (3, Empty, Empty)),
                       Node (4, Node (5, Empty, Empty), Empty))
      in
      check (list int) (show_binary_tree show_int tree)
        [3; 5] (leaves tree));

  ("internals", `Quick, fun () ->
      let tree = Node (1, Node (2, Node (3, Empty, Empty),
                                Node (4, Empty, Empty)),
                       Empty)
      in
      check (list int) (show_binary_tree show_int tree)
        [2] (internals tree));

  ("at_level", `Quick, fun () ->
      let tree = Node (1, Node (2, Node (3, Empty, Empty), Empty),
                       Node (4, Empty, Node (5, Empty, Empty)))
      in
      check (list int) (show_binary_tree show_int tree)
        [2; 4] (at_level tree 2));

  ("is_complete_binary_tree", `Quick, fun () ->
      let tree = Node (1,
                       Node (2, Node (4, Empty, Empty), Empty),
                       Node (3, Node (5, Empty, Empty), Empty))
      in
      check bool (show_binary_tree show_int tree)
        true (is_complete_binary_tree 5 tree);

      let tree = Node (1,
                       Node (2, Node (4, Empty, Empty), Empty),
                       Node (3, Empty, Node (5, Empty, Empty)))
      in
      check bool (show_binary_tree show_int tree)
        false (is_complete_binary_tree 5 tree));

  ("complete_binary_tree", `Quick, fun () ->
      check int_binary_tree "[1; 2; 3; 4; 5; 6]"
        (Node (1, Node (2, Node (4, Empty, Empty), Node (5, Empty, Empty)),
               Node (3, Node (6, Empty, Empty), Empty)))
        (complete_binary_tree [1; 2; 3; 4; 5; 6]);

      check bool "[1; 2; 3; 4; 5]" true
        (is_complete_binary_tree 5 (complete_binary_tree [1; 2; 3; 4; 5])));

  ("layout_binary_tree_1", `Quick, fun () ->
      let tree =
        let leaf x = Node (x,Empty,Empty) in
        Node('n', Node('k', Node('c', leaf 'a',
                                 Node('h', Node('g', leaf 'e',Empty), Empty)),
                       leaf 'm'),
             Node('u', Node('p', Empty, Node('s', leaf 'q', Empty)), Empty))
      in
      check char_int_int_binary_tree
        (show_binary_tree show_char tree)
        (Node (('n', 8, 1),
               Node (('k', 6, 2),
                     Node (('c', 2, 3), Node (('a', 1, 4), Empty, Empty),
                           Node (('h', 5, 4),
                                 Node (('g', 4, 5),
                                       Node (('e', 3, 6), Empty, Empty), Empty),
                                 Empty)),
                     Node (('m', 7, 3), Empty, Empty)),
               Node (('u', 12, 2),
                     Node (('p', 9, 3), Empty,
                           Node (('s', 11, 4),
                                 Node (('q', 10, 5), Empty, Empty),
                                 Empty)),
                     Empty))
        )
        (layout_binary_tree_1 tree))
]
