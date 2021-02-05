type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr


(**
 ** 46 & 47. Truth tables for logical expressions (2 variables). (medium)
 **
 ** Define a function, table2 which returns the truth table of a given logical expression in two variables (specified as arguments). The return value must be a list of triples containing (value_of_a, value_of_b, value_of_expr).
 **)
val table2: string -> string -> bool_expr -> (bool * bool * bool) list
