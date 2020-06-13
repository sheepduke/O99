(**
 ** 1. Write a function last : 'a list -> 'a option that returns the last element of a list. (easy)
 **
 ** # last [ "a" ; "b" ; "c" ; "d" ];;
 ** - : string option = Some "d"
 **
 ** # last [];;
 ** - : 'a option = None *)
let rec last (list : 'a list) : ('a option) =
  match list with
    [] -> None
  | [x] -> Some x
  | _::tail -> last tail

(**
 ** 2. Find the last but one (last and penultimate) elements of a list. (easy)
 ** 
 ** # last_two [ "a" ; "b" ; "c" ; "d" ];;
 ** - : (string * string) option = Some ("c", "d")
 ** 
 ** # last_two [ "a" ];;
 ** - : (string * string) option = None *)
let rec last_two (list : 'a list) : (('a * 'a) option) =
  match list with
    [] -> None
  | [_] -> None
  | [x; y] -> Some (x, y)
  | _::rest -> last_two rest

(**
 ** 3. Find the k'th element of a list. (easy)
 ** 
 ** # at 3 [ "a" ; "b"; "c"; "d"; "e" ];;
 ** - : string option = Some "c"
 ** 
 ** # at 3 [ "a" ];;
 ** - : string option = None *)
let rec at (index : int) (list : 'a list) : 'a option =
  match list with
    [] -> None
  | _ when index <= 0 -> None
  | [x] when index == 1 -> Some x
  | _::rest -> at (index - 1) rest

(**
 ** 4. Find the number of elements of a list. (easy)
 ** OCaml standard library has List.length but we ask that you reimplement it. Bonus for a tail recursive solution.
 **
 ** # length [ "a" ; "b" ; "c"];;
 ** - : int = 3
 ** # length [];;
 ** - : int = 0 *)
let length (list : 'a list) : int =
  let rec run len list =
    match list with
      [] -> len
    | _ :: rest -> run (len + 1) rest
  in run 0 list

(**
 ** 5. Reverse a list. (easy)
 ** OCaml standard library has List.rev but we ask that you reimplement it.
 **
 ** # rev ["a" ; "b" ; "c"];;
 ** - : string list = ["c"; "b"; "a"] *)
let rev (list : 'a list) : 'a list =
  let rec run source result =
    match source with
      [] -> result
    | head :: tail -> run tail (head :: result) in
  run list []

(**
 ** 6. Find out whether a list is a palindrome. (easy)
 ** HINT: a palindrome is its own reverse.
 **
 ** # is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ];;
 ** - : bool = true
 ** # not (is_palindrome [ "a" ; "b" ]);;
 ** - : bool = true *)
let is_palidrome (list : 'a list) =
  list = rev list

(**
 ** 7. Flatten a nested list structure. (medium)
 ** 
 ** There is no nested list type in OCaml, so we need to define one
     first. A node of a nested list is either an element, or a list of
     nodes.
 ** # flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ];;
 ** - : string list = ["a"; "b"; "c"; "d"; "e"] *)
type 'a node =
  | One of 'a 
  | Many of 'a node list

let rec flatten (list : 'a node list) : 'a list =
  match list with
    [] -> []
  | One head :: tail -> head :: flatten tail
  | Many head_list :: tail -> flatten head_list @ flatten tail
