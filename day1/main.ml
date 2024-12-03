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

let distance_lists : int list -> int list -> int =
 fun xs ys -> List.fold_left2 (fun acc x y -> acc + distance x y) 0 xs ys

module IntMap = Map.Make (Int)

let add_int_to_map : int IntMap.t -> int -> int IntMap.t =
 fun map x ->
  IntMap.update x
    (fun option -> Option.some @@ (Option.value option ~default:0 + 1))
    map

let count_numbers : int list -> int IntMap.t =
 fun ints -> List.fold_left add_int_to_map IntMap.empty ints

let single_similarity : int IntMap.t -> int -> int -> int =
 fun map acc x ->
  Option.value
    (Option.map (fun count -> count * x) (IntMap.find_opt x map))
    ~default:0
  + acc

let similarity : int list -> int list -> int =
 fun xs ys ->
  let number_counts = count_numbers ys in
  List.fold_left (single_similarity number_counts) 0 xs

let () =
  let xs, ys =
    read_lines "day1/input.txt"
    |> List.map parse_line |> list_of_tuple_to_tuple_of_lists
    |> sort_tuple_of_lists
  in
  let distance = distance_lists xs ys in
  let similarity = similarity xs ys in
  print_endline @@ string_of_int distance;
  print_endline @@ string_of_int similarity
