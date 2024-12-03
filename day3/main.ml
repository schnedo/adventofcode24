let test_input =
  "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

type parse_state =
  | Crap of string
  | MaybeMul of string
  | Mul of string
  | First of int * string

let remove_first (s : string) (n : int) = String.sub s n @@ String.length s

let rec take_while ?(matched = "") (should_take : char -> bool) (input : string)
    =
  if String.length input = 0 then (matched, input)
  else
    let first_char = String.get input 0 in
    if should_take first_char then
      let new_matched = matched ^ String.make 1 first_char in
      take_while ~matched:new_matched should_take (remove_first input 1)
    else (matched, input)

let test_number (input : string) =
  let matched, rest = input |> take_while @@ String.contains "1234567890" in
  if matched = "" then Error input else Ok (int_of_string matched, rest)

let test_tag (token : string) (input : string) =
  let token_length = String.length token in
  if
    String.length input < token_length
    || not (String.sub input 0 token_length = token)
  then Error input
  else Ok (String.sub input 0 token_length, remove_first input token_length)

let test_crap = take_while (fun c -> c != 'm')

let rec parse_input ?(akku = 0) (input : parse_state) =
  match input with
  | Crap input ->
      if input = "" then akku
      else
        let _, rest = test_crap input in
        parse_input ~akku (MaybeMul rest)
  | MaybeMul input -> (
      match test_tag "mul(" input with
      | Ok (_, rest) -> parse_input ~akku (Mul rest)
      | Error rest -> parse_input ~akku (Crap rest))
  | Mul input -> (
      match test_number input with
      | Ok (num, rest) -> parse_input ~akku (First (num, rest))
      | Error rest -> parse_input ~akku (Crap rest))
  | First (num, input) -> 
  | _ -> akku

let () = ()
