(* Write a function last : 'a list -> 'a option
 * that returns the last element of a list.
 *
 * # last [ "a" ; "b" ; "c" ; "d" ];;
 * - : string option = Some "d"
 *
 * # last [];;
 * - : 'a option = None *)
let last (list: 'a list): ('a option) =
  let rec run list = match list with
      [] -> None
    | [x] -> Some x
    | _::tail -> run tail
  in run list


(* Find the last but one (last and penultimate) elements of a list.
 * 
 * # last_two [ "a" ; "b" ; "c" ; "d" ];;
 * - : (string * string) option = Some ("c", "d")
 * 
 * # last_two [ "a" ];;
 * - : (string * string) option = None *)
let last_two (list: 'a list): (('a * 'a) option) =
  let rec run list = match list with
      [] -> None
    | [_] -> None
    | [x; y] -> Some (x, y)
    | _::rest -> run rest
  in run list


(* Find the k'th element of a list. (easy)
 * 
 * # at 3 [ "a" ; "b"; "c"; "d"; "e" ];;
 * - : string option = Some "c"
   * 
 * # at 3 [ "a" ];;
 * - : string option = None *)
let at (index: int) (list: 'a list): 'a option =
  let rec run index list = match list with
      [] -> None
    | _ when index <= 0 -> None
    | [x] when index == 1 -> Some x
    | _::rest -> run (index - 1) rest
  in run index list
