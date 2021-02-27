type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree
[@@deriving show,equal,compare]

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


(**
 ** 58. Generate-and-test paradigm. (medium)
 **
 ** Apply the generate-and-test paradigm to construct all symmetric, completely balanced binary trees with a given number of nodes.
 **)
val sym_cbal_trees: int -> char binary_tree list


(**
 ** 59. Construct height-balanced binary trees. (medium)
 **
 ** In a height-balanced binary tree, the following property holds for every node: The height of its left subtree and the height of its right subtree are almost equal, which means their difference is not greater than one.
 **
 ** Write a function hbal_tree to construct height-balanced binary trees for a given height. The function should generate all solutions via backtracking. Put the letter 'x' as information into all nodes of the tree.
 **)
val hbal_tree: int -> char binary_tree list

(**
 ** 61. Count the leaves of a binary tree. (easy)
 **
 ** A leaf is a node with no successors. Write a function count_leaves to count them.
 **)
val count_leaves: 'a binary_tree -> int


(**
 ** 61A. Collect the leaves of a binary tree in a list. (easy)
 **
 ** A leaf is a node with no successors. Write a function leaves to collect them in a list.
 **)
val leaves: 'a binary_tree -> 'a list


(**
 ** 62. Collect the internal nodes of a binary tree in a list. (easy)
 **
 ** An internal node of a binary tree has either one or two non-empty successors. Write a function internals to collect them in a list.
 **)
val internals: 'a binary_tree -> 'a list


(**
 ** 62B. Collect the nodes at a given level in a list. (easy)
 **
 ** A node of a binary tree is at level N if the path from the root to the node has length N-1. The root node is at level 1. Write a function at_level t l to collect all nodes of the tree t at level l in a list.
 **)
val at_level: 'a binary_tree -> int -> 'a list


(**
 ** 63. Construct a complete binary tree. (medium)
 **
 ** A complete binary tree with height H is defined as follows: The levels 1,2,3,...,H-1 contain the maximum number of nodes (i.e 2i-1 at the level i, note that we start counting the levels from 1 at the root). In level H, which may contain less than the maximum possible number of nodes, all the nodes are "left-adjusted". This means that in a levelorder tree traversal all internal nodes come first, the leaves come second, and empty successors (the nil's which are not really nodes!) come last.
 **
 ** Particularly, complete binary trees are used as data structures (or addressing schemes) for heaps.
 **
 ** We can assign an address number to each node in a complete binary tree by enumerating the nodes in levelorder, starting at the root with number 1. In doing so, we realize that for every node X with address A the following property holds: The address of X's left and right successors are 2*A and 2*A+1, respectively, supposed the successors do exist. This fact can be used to elegantly construct a complete binary tree structure. Write a function is_complete_binary_tree with the following specification: is_complete_binary_tree n t returns true iff t is a complete binary tree with n nodes.
 **)
val is_complete_binary_tree: int -> 'a binary_tree -> bool

val complete_binary_tree: 'a list -> 'a binary_tree
