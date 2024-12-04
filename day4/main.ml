let test_input =
  "MMMSXXMASM\n\
   MSAMXMSMSA\n\
   AMXSXMAAMM\n\
   MSAMASMSMX\n\
   XMASAMXAMM\n\
   XXAMMXXAMA\n\
   SMSMSASXSS\n\
   SAXAMASAAA\n\
   MAMMMXMMMM\n\
   MXMXAXMASX"

let to_lines = String.split_on_char '\n'
let line_to_list (line : string) = List.of_seq (String.to_seq line)
let split_lines = List.map line_to_list

type letter = X | M | A | S | Garbage

let to_letter c =
  match c with 'X' -> X | 'M' -> M | 'A' -> A | 'S' -> S | _ -> Garbage

let to_grid_line = List.map to_letter
let to_grid = List.map to_grid_line
let range n = List.init n Fun.id

class puzzle input =
  let grid = input |> to_lines |> split_lines |> to_grid in
  object (self)
    val height = List.length grid
    val width = List.length (List.nth grid 0)
    val grid = grid
    val xmas_length = String.length "xmas"
    method private elem i (j : int) = List.nth (List.nth grid i) j
    method private row_of i (j : int) = (List.nth grid i, j)

    method private column_of (i : int) j =
      (List.map (fun row -> List.nth row j) grid, i)

    method private diag_tr_bl_of i j =
      let invariant = i + j in
      ( List.fold_left
          (fun acc idx ->
            let row_idx = invariant - idx in
            if row_idx > height || idx > width then acc
            else self#elem row_idx idx :: acc)
          []
        @@ range invariant,
        invariant - j )

    method private diag_tl_br_of i j =
      let diff = i - j in
      if diff > 0 then
        let n_elements = Int.min width (height - diff) in
        let idxs = List.init n_elements (fun idx -> (idx + diff, idx)) in
        ( List.map
            (fun (row_idx, column_idx) -> self#elem row_idx column_idx)
            idxs,
          j )
      else
        let n_elements = Int.min (width + diff) height in
        let idxs = List.init n_elements (fun idx -> (idx, idx - diff)) in
        ( List.map
            (fun (row_idx, column_idx) -> self#elem row_idx column_idx)
            idxs,
          i )

    method private xs =
      let (xs : (int * int) list ref) = ref [] in
      for i = 0 to height do
        for j = 0 to width do
          if self#elem i j = X then xs := (i, j) :: !xs
        done
      done;
      xs.contents

    method private look_along (way : int -> int -> letter list * int) direction
        i j =
      let way, pos = way i j in
      if direction < 0 && pos < 3 then false
      else
        match
          ( List.nth_opt way @@ (pos + (1 * direction)),
            List.nth_opt way @@ (pos + (2 * direction)),
            List.nth_opt way @@ (pos + (3 * direction)) )
        with
        | Some M, Some A, Some S -> true
        | _ -> false

    method is_xmas vertical horizontal i j =
      let up_down = i + (vertical * 3) in
      let left_right = j + (horizontal * 3) in
      if
        up_down < xmas_length
        || up_down > height - xmas_length
        || left_right > width - xmas_length
        || left_right < xmas_length
      then false
      else
        match
          ( self#elem (i + (1 * vertical)) (j + (1 * horizontal)),
            self#elem (i + (2 * vertical)) (j + (2 * horizontal)),
            self#elem (i + (3 * vertical)) (j + (3 * horizontal)) )
        with
        | M, A, S -> true
        | _ -> false

    method private is_xmas_above = self#is_xmas 1 0
    method private is_xmas_below = self#is_xmas (-1) 0
    method private is_xmas_right = self#is_xmas 0 1
    method private is_xmas_left = self#is_xmas 0 (-1)
    method private is_xmas_tr = self#is_xmas 1 1
    method private is_xmas_tl = self#is_xmas 1 (-1)
    method private is_xmas_bl = self#is_xmas (-1) (-1)
    method private is_xmas_br = self#is_xmas (-1) 1

    method private count_xmas_of_x i j =
      List.length @@ List.filter Fun.id
      @@ List.map
           (fun f -> f i j)
           [
             self#is_xmas_above;
             self#is_xmas_below;
             self#is_xmas_right;
             self#is_xmas_left;
             self#is_xmas_tr;
             self#is_xmas_tl;
             self#is_xmas_bl;
             self#is_xmas_br;
           ]

    method count_xmas =
      List.fold_left ( + ) 0 @@ List.concat
      @@ List.mapi
           (fun i row ->
             List.mapi
               (fun j c -> if c = X then self#count_xmas_of_x i j else 0)
               row)
           grid
  end

let () =
  let puzzle = new puzzle test_input in
  print_int @@ puzzle#count_xmas;
  print_newline ()
