type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree
[@@deriving equal,compare]

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
