(*let test_input =*)
(*  "7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9"*)
(**)
(*let record_lines = String.split_on_char '\n' test_input*)

let read_lines : string -> string list =
 fun file -> In_channel.with_open_bin file In_channel.input_lines

let record_lines = read_lines "day2/input.txt"

let record_line_to_record (line : string) =
  List.map int_of_string @@ String.split_on_char ' ' line

let safe_increasing (diff : int) = diff > 0 && diff < 4
let safe_decreasing (diff : int) = diff < 0 && diff > -4

let rec is (safe_step : int -> bool) ?(try_again = true) (record : int list) =
  match record with
  | first :: second :: tail -> (
      let diff = second - first in
      match (safe_step diff, try_again) with
      | true, _ -> is safe_step ~try_again (second :: tail)
      | false, true -> is safe_step ~try_again:false (first :: tail)
      | false, false -> false)
  | _ -> true

let is_safe_single_try (record : int list) =
  is safe_decreasing ~try_again:false record
  || is safe_increasing ~try_again:false record

let is_safe (record : int list) =
  is safe_decreasing record || is safe_increasing record
  || match record with [] -> true | _ :: tail -> is_safe_single_try tail

let records = List.map record_line_to_record record_lines
let safe_records = List.filter is_safe records

let suspiciously_safe_records =
  List.filter (fun record -> not @@ is_safe_single_try record) safe_records

let print_record =
  List.iter (fun n ->
      print_int n;
      print_char ' ')

let () =
  (*List.iter*)
  (*  (fun record ->*)
  (*    print_record record;*)
  (*    print_newline ())*)
  (*  records;*)
  (*print_newline ();*)
  List.iter
    (fun record ->
      print_record record;
      print_newline ())
    suspiciously_safe_records;
  print_newline ();
  string_of_int @@ List.length safe_records |> print_endline
