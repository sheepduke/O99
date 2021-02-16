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


(**
 ** 56. Symmetric binary trees. (medium)
 **
 ** Let us call a binary tree symmetric if you can draw a vertical line through the root node and then the right subtree is the mirror image of the left subtree. Write a function is_symmetric to check whether a given binary tree is symmetric.
 **
 ** Hint: Write a function is_mirror first to check whether one tree is the mirror image of another. We are only interested in the structure, not in the contents of the nodes.
 **)
val is_symmetric: 'a binary_tree -> bool


(**
 ** 57. Binary search trees (dictionaries). (medium)
 **
 ** Construct a binary search tree from a list of integer numbers.
 **)
val construct: 'a list -> 'a binary_tree
