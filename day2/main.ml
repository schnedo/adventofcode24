let read_lines : string -> string list =
 fun file -> In_channel.with_open_bin file In_channel.input_lines

let record_lines = read_lines "day2/input.txt"

let record_line_to_record (line : string) =
  List.map int_of_string @@ String.split_on_char ' ' line

let safe_increasing (diff : int) = diff > 0 && diff < 4
let safe_decreasing (diff : int) = diff < 0 && diff > -4

let rec is (safe_direction : int -> bool) (record : int list) =
  match record with
  | first :: second :: tail ->
      let diff = second - first in
      if not @@ safe_direction diff then false
      else is safe_direction @@ (second :: tail)
  | _ -> true

let is_safe (record : int list) =
  is safe_decreasing record || is safe_increasing record

let records = List.map record_line_to_record record_lines
let safe_records = List.filter is_safe records
let () = string_of_int @@ List.length safe_records |> print_endline
