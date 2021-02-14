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


(**
 ** 48. Truth tables for logical expressions. (medium)
 **
 ** Generalize the previous problem in such a way that the logical expression may contain any number of logical variables. Define table in a way that table variables expr returns the truth table for the expression expr, which contains the logical variables enumerated in variables.
 **)
val table: string list -> bool_expr -> (((string * bool) list) * bool) list


(**
 ** 49. Gray code. (medium)
 **
 ** An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules. For example,
 **
 ** n = 1: C(1) = ['0','1'].
 ** n = 2: C(2) = ['00','01','11','10'].
 ** n = 3: C(3) = ['000','001','011','010',´110´,´111´,´101´,´100´].
 **
 ** Find out the construction rules and write a function with the following specification: gray n returns the n-bit Gray code.
 **)
val gray: int -> string list
