include Fmt

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
  | [] -> None
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
  | [] -> None
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
  | [] -> None
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
  let rec aux len list =
    match list with
    |  [] -> len
    | _ :: rest -> aux (len + 1) rest
  in aux 0 list

(**
 ** 5. Reverse a list. (easy)
 ** OCaml standard library has List.rev but we ask that you reimplement it.
 **
 ** # rev ["a" ; "b" ; "c"];;
 ** - : string list = ["c"; "b"; "a"] *)
let rev (list : 'a list) : 'a list =
  let rec aux source result =
    match source with
    | [] -> result
    | head :: tail -> aux tail (head :: result)
  in aux list []

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
  | [] -> []
  | One head :: tail -> head :: flatten tail
  | Many head_list :: tail -> flatten head_list @ flatten tail


(**
 ** 8. Eliminate consecutive duplicates of list elements. (medium)

 ** # compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
 ** - : string list = ["a"; "b"; "c"; "a"; "d"; "e"] *)
let rec compress (list : 'a list) : 'a list =
  match list with
  | x :: (y :: _ as tail) -> if x = y then compress tail else x :: compress tail
  | any -> any

(**
 ** 9. Pack consecutive duplicates of list elements into sublists. (medium)
 **
 ** # pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"];;
 ** - : string list list =
 ** [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
 **  ["e"; "e"; "e"; "e"]] *)
let pack (list : 'a list) : ('a list list) =
  let rec aux last result list =
    match list, last with
    | head :: tail, [] -> aux [head] result tail
    | head :: tail, x :: _ when head = x -> aux (head :: last) result tail
    | head :: tail, x :: _ when head <> x -> aux [head] (result @ [last]) tail
    | _, _ -> result @ [last]
  in match list with
  | [] -> []
  | list -> aux [] [] list

(**
 ** 10. Run-length encoding of a list. (easy)
 **
 ** If you need so, refresh your memory about run-length encoding.
 **
 ** Here is an example:
 **
 ** # encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
 ** - : (int * string) list =
 ** [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")] *)
let encode (list : 'a list) : ((int * 'a) list) =
  let rec aux last result list =
    match list, last with
    | head :: tail, None -> aux (Some (1, head)) result tail
    | head :: tail, Some (count, value) when head = value ->
      aux (Some (count + 1, value)) result tail
    | head :: tail, Some (count, value) when head <> value ->
      aux (Some (1, head)) (result @ [(count, value)]) tail
    | _, None -> result
    | _, Some pair -> result @ [pair]
  in aux None [] list

(**
 ** 11. Modified run-length encoding. (easy)
 **
 ** Modify the result of the previous problem in such a way that if an element
 ** has no duplicates it is simply copied into the result list. Only elements
 ** with duplicates are transferred as (N E) lists.
 **
 ** Since OCaml lists are homogeneous, one needs to define a type to hold both
 ** single elements and sub-lists.
 **
 ** # encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
 ** - : string rle list =
 ** [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 **   Many (4, "e")] *)
type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode2 (list : 'a list) : 'a rle list =
  let to_rle count value =
    if count = 1 then (One value) else (Many (count, value)) in
  let rec aux count result list =
    match list with
    | head :: [] -> result @ [to_rle (count + 1) head]
    | x :: (y :: _ as tail) ->
      if x = y then aux (count + 1) result tail
      else aux 0 (result @ [to_rle (count + 1) x]) tail
    | _ -> [] in
  aux 0 [] list

(**
 ** 12. Decode a run-length encoded list. (medium)
 **
 ** Given a run-length code list generated as specified in the previous problem,
 ** construct its uncompressed version.
 ** # decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")];;
 ** - : string list = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] *)
let rec decode (rle_list : 'a rle list) =
  let rec make_list result count value =
    assert (count >= 0);
    if count = 0 then result else make_list (value :: result) (count - 1) value in
  let rle_to_list = function
    | One value -> [value]
    | Many (count, value) -> make_list [] count value in
  match rle_list with
  | [] -> []
  | head :: tail -> (rle_to_list head) @ (decode tail)
