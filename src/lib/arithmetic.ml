let is_prime: int -> bool = fun num ->
  let first = 2 in
  let last = min (num - 1) (Float.to_int (ceil (sqrt (Int.to_float num)))) in
  try
    for i = first to last do
      if num mod i = 0 then raise Exit
    done;
    true
  with Exit -> false


let gcd: int -> int -> int = fun x y ->
  let rec aux x y =
    if x = y
    then x
    else if x > y then aux (x - y) y else aux x (y - x)
  in
  aux x y
