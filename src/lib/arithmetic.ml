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


let all_primes: int -> int -> int list = fun start_num end_num ->
  let stream = Stream.from (fun x ->
      if x + start_num <= end_num
      then Some (x + start_num)
      else None)
  in
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
  let all_primes = all_primes 2 num in
  let rec aux num primes result =
    match primes with
    | [] -> result
    | head :: rest ->
      if num mod head = 0
      then aux (num / head) all_primes (head :: result)
      else aux num rest result
  in
  List.rev (aux num all_primes [])


let factors2: int -> (int * int) list = fun num ->
  let all_primes = all_primes 2 num in
  let rec aux num primes result =
    match primes with
    | [] -> result
    | head :: rest ->
      if num mod head = 0
      then let result' = match result with
          | (factor, count) :: tail ->
            if head = factor
            then (head, count + 1) :: tail
            else (head, 1) :: result
          | [] -> [head, 1]
        in
        aux (num / head) all_primes result'
      else aux num rest result
  in
  List.rev (aux num all_primes [])


let phi_improved: int -> int = fun num ->
  Stdlib.List.fold_left (fun acc (x, y) ->
      let p, m = (Float.of_int x, Float.of_int y) in
      acc *. (p -. 1.) *. (p ** (m -. 1.)))
    1.0
    (factors2 num)
  |> Int.of_float


let timeit: (int -> int) -> int -> float = fun func arg ->
  let start_time = Unix.gettimeofday () in
  let _ = func arg in
  let end_time = Unix.gettimeofday () in
  end_time -. start_time


let goldbach: int -> (int * int) = fun num ->
  assert (num > 2 && num mod 2 = 0);
  let rec aux cur = 
    if is_prime cur && is_prime (num - cur)
    then cur, num - cur
    else aux (cur + 1)
  in
  aux 2


let goldbach_list: int -> int -> (int * (int * int)) list = fun low high ->
  let rec aux cur result =
    if cur > high then result
    else if cur mod 2 <> 0 then aux (cur + 1) result
    else aux (cur + 2) ((cur, goldbach cur) :: result)
  in
  List.rev (aux low [])
