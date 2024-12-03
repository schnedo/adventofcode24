let read_lines : string -> string list =
 fun file -> In_channel.with_open_bin file In_channel.input_lines

let record_lines = read_lines "day2/input.txt"

let record_line_to_record (line : string) =
  List.map int_of_string @@ String.split_on_char ' ' line

let rec diffs (record : int list) =
  match record with
  | first :: second :: tail -> (first - second) :: diffs (second :: tail)
  | _ -> []

let is_increasing = List.for_all (fun num -> num > 0 && num < 4)
let is_decreasing = List.for_all (fun num -> num < 0 && num > -4)

let is_safe (record : int list) =
  let d = diffs record in
  is_increasing d || is_decreasing d

let records = List.map record_line_to_record record_lines
let safe_records = List.filter is_safe records
let () = string_of_int @@ List.length safe_records |> print_endline
