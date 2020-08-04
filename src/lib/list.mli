(**
 ** 1. Write a function last : 'a list -> 'a option that returns the last element of a list. (easy)
 **
 ** # last [ "a" ; "b" ; "c" ; "d" ];;
 ** - : string option = Some "d"
 **
 ** # last [];;
 ** - : 'a option = None *)
val last : 'a list -> 'a option


(**
 ** 2. Find the last but one (last and penultimate) elements of a list. (easy)
 ** 
 ** # last_two [ "a" ; "b" ; "c" ; "d" ];;
 ** - : (string * string) option = Some ("c", "d")
 ** 
 ** # last_two [ "a" ];;
 ** - : (string * string) option = None *)
val last_two : 'a list -> ('a * 'a) option


(**
 ** 3. Find the k'th element of a list. (easy)
 ** 
 ** # at 3 [ "a" ; "b"; "c"; "d"; "e" ];;
 ** - : string option = Some "c"
 ** 
 ** # at 3 [ "a" ];;
 ** - : string option = None *)
val at : int -> 'a list -> 'a option


(**
 ** 4. Find the number of elements of a list. (easy)
 ** OCaml standard library has List.length but we ask that you reimplement it. Bonus for a tail recursive solution.
 **
 ** # length [ "a" ; "b" ; "c"];;
 ** - : int = 3
 ** # length [];;
 ** - : int = 0 *)
val length : 'a list -> int


(**
 ** 5. Reverse a list. (easy)
 ** OCaml standard library has List.rev but we ask that you reimplement it.
 **
 ** # rev ["a" ; "b" ; "c"];;
 ** - : string list = ["c"; "b"; "a"] *)
val rev : 'a list -> 'a list


(**
 ** 6. Find out whether a list is a palindrome. (easy)
 ** HINT: a palindrome is its own reverse.
 **
 ** # is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ];;
 ** - : bool = true
 ** # not (is_palindrome [ "a" ; "b" ]);;
 ** - : bool = true *)
val is_palidrome : 'a list -> bool


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

val flatten : 'a node list -> 'a list


(**
 ** 8. Eliminate consecutive duplicates of list elements. (medium)

 ** # compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
 ** - : string list = ["a"; "b"; "c"; "a"; "d"; "e"] *)
val compress : 'a list -> 'a list


(**
 ** 9. Pack consecutive duplicates of list elements into sublists. (medium)
 **
 ** # pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"];;
 ** - : string list list =
 ** [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
 **  ["e"; "e"; "e"; "e"]] *)
val pack : 'a list -> 'a list list


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
val encode : 'a list -> (int * 'a) list


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
 ** # encode2 ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
 ** - : string rle list =
 ** [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 **   Many (4, "e")] *)
type 'a rle =
  | One of 'a
  | Many of int * 'a

val encode2 : 'a list -> 'a rle list


(**
 ** 12. Decode a run-length encoded list. (medium)
 **
 ** Given a run-length code list generated as specified in the previous problem,
 ** construct its uncompressed version.
 ** # decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")];;
 ** - : string list = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] *)
val decode : 'a rle list -> 'a list


(** 14. Duplicate the elements of a list. (easy)
 **
 ** # duplicate ["a";"b";"c";"c";"d"];;
 ** - : string list = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"] *)
val duplicate : 'a list -> 'a list


(** 15. Replicate the elements of a list a given number of times. (medium)
 **
 ** # replicate ["a";"b";"c"] 3;;
 ** - : string list = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"] *)
val replicate : 'a list -> int -> 'a list


(** 16. Drop every N'th element from a list. (medium)
 **
 ** # drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
 ** - : string list = ["a"; "b"; "d"; "e"; "g"; "h"; "j"] *)
val drop : 'a list -> int -> 'a list


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
val split : 'a list -> int -> 'a list * 'a list


(** 18. Extract a slice from a list. (medium)
 **
 ** Given two indices, i and k, the slice is the list containing the elements
 ** between the i'th and k'th element of the original list (both limits
 ** included). Start counting the elements with 0 (this is the way the List
 ** module numbers elements).
 **
 ** # slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6;;
 ** - : string list = ["c"; "d"; "e"; "f"; "g"] *)
val slice : 'a list -> int -> int -> 'a list


(** 19. Rotate a list N places to the left. (medium)
 **
 ** # rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
 ** - : string list = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]
 ** # rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2);;
 ** - : string list = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"] *)
val rotate : 'a list -> int -> 'a list


(** 20. Remove the K'th element from a list. (easy)
 **
 ** The first element of the list is numbered 0, the second 1,...
 **
 ** # remove_at 1 ["a";"b";"c";"d"];;
 ** - : string list = ["a"; "c"; "d"] *)
val remove_at : int -> 'a list -> 'a list


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
val insert_at : 'a -> int -> 'a list -> 'a list


(** 22. Create a list containing all integers within a given range. (easy)
 ** 
 ** If first argument is greater than second, produce a list in decreasing order.
 **
 ** # range 4 9;;
 ** - : int list = [4; 5; 6; 7; 8; 9]
 ** # range 9 4;;
 ** - : int list = [9; 8; 7; 6; 5; 4] *)
val range : int -> int -> int list

(** 23. Extract a given number of randomly selected elements from a list. (medium)
 **
 ** The selected items shall be returned in a list. We use the Random module
 ** but do not initialize it with Random.self_init for reproducibility.
 **
 ** # rand_select ["a";"b";"c";"d";"e";"f";"g";"h"] 3;;
 ** - : string list = ["g"; "d"; "a"] *)
val rand_select : 'a list -> int -> 'a list


(** 24. Lotto: Draw N different random numbers from the set 1..M. (easy)
 **
 ** The selected numbers shall be returned in a list.
 ** 
 ** # lotto_select 6 49;;
 ** - : int list = [10; 20; 44; 22; 41; 2] *)
val lotto_select : int -> int -> int list


(** 25. Generate a random permutation of the elements of a list. (easy)
 ** 
 ** # permutation ["a"; "b"; "c"; "d"; "e"; "f"];;
 ** - : string list = ["a"; "e"; "f"; "b"; "d"; "c"] *)
val permutation : 'a list -> 'a list


(** 26. Generate the combinations of K distinct objects chosen from the N elements
 ** of a list. (medium)
 **
 ** In how many ways can a committee of 3 be chosen from a group of 12 people?
 ** We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the
 ** well-known binomial coefficients). For pure mathematicians, this result may
 ** be great. But we want to really generate all the possibilities in a
 ** list.
 **
 ** # extract 2 ["a";"b";"c";"d"];;
 ** - : string list list =
 **   [["a"; "b"]; ["a"; "c"]; ["a"; "d"]; ["b"; "c"]; ["b"; "d"]; ["c"; "d"]] *)
val extract : int -> 'a list -> 'a list list
