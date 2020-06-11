let last (lst: 'a list): ('a option) =
  let rec doit list = match list with
      [] -> None
    | [x] -> Some x
    | _::tail -> doit tail
  in
  doit lst
