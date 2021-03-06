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


let table: string list -> bool_expr -> (((string * bool) list) * bool) list =
  fun var_list expr ->
  let rec get_possible_values (count: int): bool list list =
    assert (count >= 1);
    match count with
    | 1 -> [[true]; [false]]
    | _ -> Stdlib.List.fold_left (fun acc list ->
        acc @ [list @ [true]; list @ [false]])
        [] (get_possible_values (count - 1))
  in
  let rec zip a b =
    match a, b with
    | [], [] -> []
    | head1 :: rest1, head2 :: rest2 -> (head1, head2) :: (zip rest1 rest2)
    | _, _ -> failwith "Invalid argument"
  in
  let make_pairs vars val_list =
    Stdlib.List.fold_left (fun acc list ->
        acc @ [zip vars list])
      [] val_list
  in
  let rec eval vars expr =
    match expr with
    | Var var -> Stdlib.List.assoc var vars
    | Not e -> eval vars e
    | And (e1, e2) -> (eval vars e1) && (eval vars e2)
    | Or (e1, e2) -> (eval vars e1) || (eval vars e2)
  in
  Stdlib.List.length var_list
  |> get_possible_values
  |> make_pairs var_list
  |> Stdlib.List.map (fun vars -> vars, (eval vars expr))


let gray: int -> string list = fun n ->
  assert (n > 0);
  let rec aux i result = 
    if i = n
    then result
    else aux (i + 1)
        ((Stdlib.List.map (fun x -> "0" ^ x) result)
         @
         (Stdlib.List.map (fun x -> "1" ^ x) (List.rev result)))
  in
  aux 1 ["0"; "1"]
