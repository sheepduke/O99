include List
open Printf

let () = let x = List.last [1; 2; 3; 4]
  in match x with
    None -> printf "Nothing"
  | Some num -> printf "%d" num
