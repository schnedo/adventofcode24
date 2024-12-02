let read_lines : string -> string list =
 fun file -> In_channel.with_open_bin file In_channel.input_lines

let parse_line : string -> int * int =
 fun line ->
  (int_of_string @@ String.sub line 0 5, int_of_string @@ String.sub line 8 5)

let list_of_tuple_to_tuple_of_lists : (int * int) list -> int list * int list =
 fun tuples ->
  List.fold_right
    (fun (a, b) (a_list, b_list) -> (a :: a_list, b :: b_list))
    tuples ([], [])

let sort_list : 'a list -> 'a list = fun list -> List.sort compare list

let sort_tuple_of_lists : int list * int list -> int list * int list =
 fun (a_list, b_list) -> (sort_list a_list, sort_list b_list)

let distance : int -> int -> int =
 fun a b ->
  let diff = a - b in
  if diff < 0 then -diff else diff

let distance_lists : int list * int list -> int =
 fun (xs, ys) -> List.fold_left2 (fun acc x y -> acc + distance x y) 0 xs ys

let () =
  let distance =
    read_lines "day1/input.txt"
    |> List.map parse_line |> list_of_tuple_to_tuple_of_lists
    |> sort_tuple_of_lists |> distance_lists
  in
  print_endline @@ string_of_int distance
