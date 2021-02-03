let is_prime: int -> bool = fun num ->
  if num < 2 then false
  else 
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


let coprime: int -> int -> bool = fun x y ->
  (gcd x y) = 1


let phi: int -> int = fun num ->
  let stream = Stream.from (fun x -> if x < num - 1 then Some(x + 1) else None) in
  let rec aux count =
    try
      if coprime num (Stream.next stream)
      then aux (count + 1)
      else aux count
    with Stream.Failure -> count
  in
  aux 0


let get_primes: int -> int list = fun num ->
  let stream = Stream.from (fun x -> if x <= num then Some x else None) in
  let rec aux result =
    try
      let cur = Stream.next stream in
      if is_prime cur
      then aux (result @ [cur])
      else aux result
    with Stream.Failure -> result
  in
  aux []

let factors: int -> int list = fun num ->
  let all_primes = get_primes num in
  let rec aux num primes result =
    match primes with
    | [] -> result
    | head :: rest ->
      if num mod head = 0
      then aux (num / head) all_primes (head :: result)
      else aux num rest result
  in
  List.rev (aux num all_primes [])
