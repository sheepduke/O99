include Fmt

(**
 ** 1. Write a function last : 'a list -> 'a option that returns the last element of a list. (easy)
 **
 ** # last [ "a" ; "b" ; "c" ; "d" ];;
 ** - : string option = Some "d"
 **
 ** # last [];;
 ** - : 'a option = None *)
let rec last (list : 'a list) : 'a option =
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
let rec last_two (list : 'a list) : ('a * 'a) option =
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
  in
  aux 0 list

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
  in
  aux list []

(**
 ** 6. Find out whether a list is a palindrome. (easy)
 ** HINT: a palindrome is its own reverse.
 **
 ** # is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ];;
 ** - : bool = true
 ** # not (is_palindrome [ "a" ; "b" ]);;
 ** - : bool = true *)
let is_palidrome (list : 'a list) : bool =
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
let pack (list : 'a list) : 'a list list =
  let rec aux last result list =
    match list, last with
    | head :: tail, [] -> aux [head] result tail
    | head :: tail, x :: _ when head = x -> aux (head :: last) result tail
    | head :: tail, x :: _ when head <> x -> aux [head] (result @ [last]) tail
    | _, _ -> result @ [last]
  in
  match list with
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
let encode (list : 'a list) : (int * 'a) list =
  let rec aux last result list =
    match list, last with
    | head :: tail, None -> aux (Some (1, head)) result tail
    | head :: tail, Some (count, value) when head = value ->
      aux (Some (count + 1, value)) result tail
    | head :: tail, Some (count, value) when head <> value ->
      aux (Some (1, head)) (result @ [(count, value)]) tail
    | _, None -> result
    | _, Some pair -> result @ [pair]
  in
  aux None [] list

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
    if count = 1 then (One value) else (Many (count, value))
  in
  let rec aux count result list =
    match list with
    | head :: [] -> result @ [to_rle (count + 1) head]
    | x :: (y :: _ as tail) ->
      if x = y then aux (count + 1) result tail
      else aux 0 (result @ [to_rle (count + 1) x]) tail
    | _ -> []
  in
  aux 0 [] list

(**
 ** 12. Decode a run-length encoded list. (medium)
 **
 ** Given a run-length code list generated as specified in the previous problem,
 ** construct its uncompressed version.
 ** # decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")];;
 ** - : string list = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] *)
let make_list (value : 'a) (count : int) : 'a list =
  let rec aux result value count =
    assert (count >= 0);
    if count = 0 then result else aux (value :: result) value (count - 1)
  in
  aux [] value count

let rec decode (rle_list : 'a rle list) : 'a list =
  let rle_to_list = function
    | One value -> [value]
    | Many (count, value) -> make_list value count
  in
  match rle_list with
  | [] -> []
  | head :: tail -> (rle_to_list head) @ (decode tail)

(** 14. Duplicate the elements of a list. (easy)
 **
 ** # duplicate ["a";"b";"c";"c";"d"];;
 ** - : string list = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"] *)
let duplicate (list : 'a list) : 'a list =
  let rec aux result list =
    match list with
    | [] -> result
    | head :: tail -> aux (result @ [head; head]) tail
  in
  aux [] list

(** 15. Replicate the elements of a list a given number of times. (medium)
 **
 ** # replicate ["a";"b";"c"] 3;;
 ** - : string list = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"] *)
let replicate (list : 'a list) (count : int) : 'a list =
  let rec aux result list =
    match list with
    | [] -> result
    | head :: tail -> aux (result @ (make_list head count)) tail
  in
  aux [] list

(** 16. Drop every N'th element from a list. (medium)
 **
 ** # drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
 ** - : string list = ["a"; "b"; "d"; "e"; "g"; "h"; "j"] *)
let drop (list : 'a list) (every : int) : 'a list =
  let rec aux result num list = 
    match list, num with
    | [], _ -> result
    | _ :: tail, 1 -> aux result every tail
    | head :: tail, num -> aux (result @ [head]) (num - 1) tail
  in
  aux [] every list

(** 17. Split a list into two parts; the length of the first part is given. (easy)
 **
 ** If the length of the first part is longer than the entire list, then the
    first part is the list and the second part is empty.
 **
 ** # split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
 ** - : string list * string list =
 **   (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])
 **
 ** # split ["a";"b";"c";"d"] 5;;
 ** - : string list * string list = (["a"; "b"; "c"; "d"], []) *)
let split (list : 'a list) (count : int) : 'a list * 'a list =
  let rec aux first second count =
    match first, second, count with
    | _, [], _ -> first, []
    | _, _, 0 -> first, second
    | _, head :: tail, _ -> aux (first @ [head]) tail (count - 1)
  in
  aux [] list count

(** 18. Extract a slice from a list. (medium)
 **
 ** Given two indices, i and k, the slice is the list containing the elements
 ** between the i'th and k'th element of the original list (both limits
 ** included). Start counting the elements with 0 (this is the way the List
 ** module numbers elements).
 **
 ** # slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6;;
 ** - : string list = ["c"; "d"; "e"; "f"; "g"] *)
let slice (list : 'a list) (first : int) (last : int) : 'a list =
  let rec aux result index list =
    match list, index with
    | _ :: tail, _ when index < first -> aux result (index + 1) tail
    | head :: tail, _ when index >= first && index <= last ->
      aux (result @ [head]) (index + 1) tail
    | _, _ -> result
  in
  aux [] 0 list

(** 19. Rotate a list N places to the left. (medium)
 **
 ** # rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
 ** - : string list = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]
 ** # rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2);;
 ** - : string list = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"] *)
let rotate (list : 'a list) (count : int) : 'a list =
  let rec aux left right count =
    match right with
    | head :: tail when count > 0 -> aux (left @ [head]) tail (count - 1)
    | _ -> right @ left
  in
  aux [] list count

(** 20. Remove the K'th element from a list. (easy)
 **
 ** The first element of the list is numbered 0, the second 1,...
 **
 ** # remove_at 1 ["a";"b";"c";"d"];;
 ** - : string list = ["a"; "c"; "d"] *)
let remove_at (index : int) (list : 'a list) : 'a list =
  let rec aux left right index =
    match right with
    | head :: tail when index > 0 -> aux (left @ [head]) tail (index - 1)
    | _ :: tail when index = 0 -> left @ tail
    | _ -> left @ right
  in
  aux [] list index

(** 21. Insert an element at a given position into a list. (easy)
 **
 ** Start counting list elements with 0. If the position is larger or equal to
 ** the length of the list, insert the element at the end. (The behavior is
 ** unspecified if the position is negative.)
 ** 
 ** # insert_at "alfa" 1 ["a";"b";"c";"d"];;
 ** - : string list = ["a"; "alfa"; "b"; "c"; "d"]
 ** # insert_at "alfa" 3 ["a";"b";"c";"d"];;
 ** - : string list = ["a"; "b"; "c"; "alfa"; "d"]
 ** # insert_at "alfa" 4 ["a";"b";"c";"d"];;
 ** - : string list = ["a"; "b"; "c"; "d"; "alfa"] *)
let rec insert_at (element : 'a) (index : int) (list : 'a list) : 'a list =
  match list with
  | [] -> [element]
  | head :: tail -> if index = 0
    then element :: list
    else head :: insert_at element (index - 1) tail

(** 22. Create a list containing all integers within a given range. (easy)
 ** 
 ** If first argument is greater than second, produce a list in decreasing order.
 **
 ** # range 4 9;;
 ** - : int list = [4; 5; 6; 7; 8; 9]
 ** # range 9 4;;
 ** - : int list = [9; 8; 7; 6; 5; 4] *)
let range (left : int) (right : int) : int list =
  let rec aux left right = if left <= right
    then left :: aux (left + 1) right
    else []
  in
  if left <= right then aux left right else rev (aux right left)
