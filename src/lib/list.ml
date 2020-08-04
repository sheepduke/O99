open Base

let rec last list =
  match list with
  | [] -> None
  | [x] -> Some x
  | _::tail -> last tail


let rec last_two list =
  match list with
  | [] -> None
  | [_] -> None
  | [x; y] -> Some (x, y)
  | _::rest -> last_two rest


let rec at index list =
  match list with
  | [] -> None
  | _ when index <= 0 -> None
  | [x] when index = 1 -> Some x
  | _::rest -> at (index - 1) rest


let length list =
  let rec aux len list =
    match list with
    |  [] -> len
    | _ :: rest -> aux (len + 1) rest
  in
  aux 0 list


let rev list =
  let rec aux source result =
    match source with
    | [] -> result
    | head :: tail -> aux tail (head :: result)
  in
  aux list []


let is_palidrome list =
  List.equal Poly.equal list (List.rev list)


type 'a node =
  | One of 'a 
  | Many of 'a node list


let rec flatten list =
  match list with
  | [] -> []
  | One head :: tail -> head :: flatten tail
  | Many head_list :: tail -> flatten head_list @ flatten tail


let rec compress list =
  match list with
  | x :: (y :: _ as tail) ->
    if Poly.equal x y
    then compress tail
    else x :: compress tail
  | any -> any


let pack list =
  let rec aux last result list =
    match list, last with
    | head :: tail, [] -> aux [head] result tail
    | head :: tail, x :: _ when Poly.equal head x ->
      aux (head :: last) result tail
    | head :: tail, x :: _ when not (Poly.equal head x) ->
      aux [head] (result @ [last]) tail
    | _, _ -> result @ [last]
  in
  match list with
  | [] -> []
  | list -> aux [] [] list


let encode list =
  let rec aux last result list =
    match list, last with
    | head :: tail, None -> aux (Some (1, head)) result tail
    | head :: tail, Some (count, value) when Poly.equal head value ->
      aux (Some (count + 1, value)) result tail
    | head :: tail, Some (count, value) when not (Poly.equal head value) ->
      aux (Some (1, head)) (result @ [(count, value)]) tail
    | _, None -> result
    | _, Some pair -> result @ [pair]
  in
  aux None [] list


type 'a rle =
  | One of 'a
  | Many of int * 'a


let encode2 list =
  let to_rle count value =
    if count = 1 then (One value) else (Many (count, value))
  in
  let rec aux count result list =
    match list with
    | head :: [] -> result @ [to_rle (count + 1) head]
    | x :: (y :: _ as tail) ->
      if Poly.equal x y then aux (count + 1) result tail
      else aux 0 (result @ [to_rle (count + 1) x]) tail
    | _ -> []
  in
  aux 0 [] list


let make_list value count =
  let rec aux result value count =
    assert (count >= 0);
    if count = 0 then result else aux (value :: result) value (count - 1)
  in
  aux [] value count


let rec decode rle_list =
  let rle_to_list = function
    | One value -> [value]
    | Many (count, value) -> make_list value count
  in
  match rle_list with
  | [] -> []
  | head :: tail -> (rle_to_list head) @ (decode tail)


let duplicate list =
  let rec aux result list =
    match list with
    | [] -> result
    | head :: tail -> aux (result @ [head; head]) tail
  in
  aux [] list


let replicate list count =
  let rec aux result list =
    match list with
    | [] -> result
    | head :: tail -> aux (result @ (make_list head count)) tail
  in
  aux [] list


let drop list every =
  let rec aux result num list = 
    match list, num with
    | [], _ -> result
    | _ :: tail, 1 -> aux result every tail
    | head :: tail, num -> aux (result @ [head]) (num - 1) tail
  in
  aux [] every list


let split list count =
  let rec aux first second count =
    match first, second, count with
    | _, [], _ -> first, []
    | _, _, 0 -> first, second
    | _, head :: tail, _ -> aux (first @ [head]) tail (count - 1)
  in
  aux [] list count


let slice list first last =
  let rec aux result index list =
    match list, index with
    | _ :: tail, _ when index < first -> aux result (index + 1) tail
    | head :: tail, _ when index >= first && index <= last ->
      aux (result @ [head]) (index + 1) tail
    | _, _ -> result
  in
  aux [] 0 list


let rotate list count =
  let rec aux left right count =
    match right with
    | head :: tail when count > 0 -> aux (left @ [head]) tail (count - 1)
    | _ -> right @ left
  in
  aux [] list count


let remove_at index list =
  let rec aux left right index =
    match right with
    | head :: tail when index > 0 -> aux (left @ [head]) tail (index - 1)
    | _ :: tail when index = 0 -> left @ tail
    | _ -> left @ right
  in
  aux [] list index


let rec insert_at element index list =
  match list with
  | [] -> [element]
  | head :: tail -> if index = 0
    then element :: list
    else head :: insert_at element (index - 1) tail


let range left right =
  let rec aux left right =
    if left <= right
    then left :: aux (left + 1) right
    else []
  in
  if left <= right then aux left right else rev (aux right left)


let lotto_select count num =
  let rec aux num count result =
    if count = 0
    then result
    else let next = Random.int num in
      if List.mem result next ~equal:(=)
      then aux num count result
      else aux num (count - 1) (next :: result)
  in
  if num < count
  then raise (Invalid_argument
                (Printf.sprintf "num %d is less than count %d" num count))
  else aux num count []


let rand_select list count =
  let indexes = lotto_select count (length list) in
  List.map indexes ~f:(fun index -> List.nth_exn list index)


let permutation list =
  let length = length list in
  let indexes = lotto_select length length in
  List.map indexes ~f:(fun index -> List.nth_exn list index)


let rec extract count list =
  if count = 0
  then [[]]
  else
    match list with
    | [] -> []
    | head :: tail ->
      List.map ~f:(fun sublist -> head :: sublist)
        (extract (count - 1) tail)
      @ (extract count tail)
