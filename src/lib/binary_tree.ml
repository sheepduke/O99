type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree
[@@deriving show,equal,compare]

let pp: (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a binary_tree -> unit = fun pp_value ppf tree ->
  let rec aux ppf tree =
    match tree with
    | Empty -> Format.fprintf ppf "Empty"
    | Node (value, left, right) ->
      Format.fprintf ppf "Node (%a, %a, %a)"
        pp_value value
        aux left
        aux right
  in
  aux ppf tree


let cbal_tree: int -> char binary_tree list = fun n ->
  assert (n >= 0);
  let product left_trees right_trees =
    Stdlib.List.map (fun left_tree ->
        Stdlib.List.map (fun right_tree -> Node ('x', left_tree, right_tree))
          right_trees)
      left_trees
    |> Stdlib.List.flatten
  in 
  let rec aux n =
    match n with
    | 0 -> [Empty]
    | 1 -> [Node ('x', Empty, Empty)]
    | _ ->
      let left = (n - 1) / 2 in
      let right = n - 1 - left in
      if left = right
      then let result = aux left in product result result
      else
        let left_result = aux left and right_result = aux right in
        (product left_result right_result) @ (product right_result left_result)
  in
  aux n


let is_symmetric: 'a binary_tree -> bool = fun tree ->
  let rec is_mirror left right =
    match left, right with
    | Empty, Empty -> true
    | Node (_, l1, r1), Node (_, l2, r2) -> is_mirror l1 r2 && is_mirror l2 r1
    | _, _ -> false
  in
  match tree with
  | Empty -> true
  | Node (_, left, right) -> is_mirror left right


let construct: 'a list -> 'a binary_tree = fun list ->
  let rec add root value =
    match root with
    | Empty -> Node (value, Empty, Empty)
    | Node (v, left, right) as node ->
      if value = v then node
      else if value < v then Node (v, add left value, right)
      else Node (v, left, add right value)
  in
  Stdlib.List.fold_left add Empty list


let sym_cbal_trees: int -> char binary_tree list = fun n ->
  assert (n >= 0);
  let rec mirror tree =
    match tree with
    | Empty -> Empty
    | Node (value, left, right) -> Node (value, (mirror right), (mirror left))
  in
  match n with
  | 0 -> []
  | 1 -> [Node ('x', Empty, Empty)]
  | _ when n mod 2 = 0 -> []
  | _ -> Stdlib.List.map
           (fun tree -> Node ('x', tree, mirror tree))
           (cbal_tree ((n - 1) / 2))


let hbal_tree: int -> char binary_tree list = fun n ->
  assert (n >= 0);
  let product left_trees right_trees =
    Stdlib.List.map (fun left_tree ->
        Stdlib.List.map (fun right_tree -> Node ('x', left_tree, right_tree))
          right_trees)
      left_trees
    |> Stdlib.List.flatten
  in
  (* The result is a pair of (n-1, n-2) results. *)
  let rec aux i (last1, last2) =
    if i > n then last1
    else
      let current =
        (product last1 last1)
        @ (product last1 last2)
        @ (product last2 last1)
      in
      aux (i + 1) (current, last1)
  in
  if n = 0 then [Empty]
  else if n = 1 then [Node ('x', Empty, Empty)]
  else aux 2 ([Node ('x', Empty, Empty)], [Empty])


let count_leaves: 'a binary_tree -> int = fun tree ->
  let rec aux tree =
    match tree with
    | Empty -> 0
    | Node (_, Empty, Empty) -> 1
    | Node (_, left, right) -> (aux left) + (aux right)
  in
  aux tree


let leaves: 'a binary_tree -> 'a list = fun tree ->
  let rec aux tree =
    match tree with
    | Empty -> []
    | Node (value, Empty, Empty) -> [value]
    | Node (_, left, right) -> (aux left) @ (aux right)
  in
  aux tree


let internals: 'a binary_tree -> 'a list = fun tree ->
  let rec aux tree result =
    match tree with
    | Empty -> result
    | Node (_, left, Empty) -> aux left result
    | Node (_, Empty, right) -> aux right result
    | Node (value, left, right) -> aux right (aux left (value :: result))
  in
  aux tree []


let at_level: 'a binary_tree -> int -> 'a list = fun tree level ->
  let rec aux tree current_level result =
    match tree with
    | Empty -> result
    | Node (value, left, right) ->
      if current_level = level
      then result @ [value]
      else aux right (current_level + 1) (aux left (current_level + 1) result)
  in
  aux tree 1 []


let is_complete_binary_tree: int -> 'a binary_tree -> bool = fun count tree ->
  let rec count_nodes tree =
    match tree with
    | Empty -> 0
    | Node (_, left, right) -> 1 + (count_nodes left) + (count_nodes right)
  in
  let rec is_complete_tree tree =
    match tree with
    | Empty -> true
    | Node (_, Empty, Node (_, _, _)) -> false
    | Node (_, left, right) ->
      (is_complete_tree left) && (is_complete_tree right)
  in
  (count_nodes tree = count) && (is_complete_tree tree)


let complete_binary_tree: 'a list -> 'a binary_tree = fun values_list ->
  (* Convert list to array for better random access perf. *)
  let values = Array.of_list values_list in
  let value_count = Array.length values in
  let rec aux index =
    if index >= value_count
    then Empty
    else begin
      let value = Array.get values index in
      let left_index = index * 2 + 1 in
      let right_index = left_index + 1 in
      Node (value, aux left_index, aux right_index)
    end
  in
  aux 0
