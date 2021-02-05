type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr


let table2: string -> string -> bool_expr -> (bool * bool * bool) list =
  fun a b expr ->
  let rec eval a_value b_value expr =
    match expr with
    | Var x when x = a -> a_value
    | Var x when x = b -> b_value
    | Var _ -> failwith "Invalid argument"
    | Not e -> not (eval a_value b_value e)
    | And (e1, e2) -> (eval a_value b_value e1) && (eval a_value b_value e2)
    | Or (e1, e2) -> (eval a_value b_value e1) || (eval a_value b_value e2)
  in
  [(true, true, (eval true true expr)); (true, false, (eval true false expr));
   (false, true, (eval false true expr)); (false, false, (eval false true expr))]
