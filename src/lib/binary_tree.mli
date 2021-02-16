type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree
[@@deriving equal,compare]

val pp: (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a binary_tree -> unit

(**
 ** 55. Construct completely balanced binary trees. (medium)
 **
 ** In a completely balanced binary tree, the following property holds for every node: The number of nodes in its left subtree and the number of nodes in its right subtree are almost equal, which means their difference is not greater than one.
 **
 ** Write a function cbal_tree to construct completely balanced binary trees for a given number of nodes. The function should generate all solutions via backtracking. Put the letter 'x' as information into all nodes of the tree.
 **)
val cbal_tree: int -> char binary_tree list
