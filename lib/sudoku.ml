exception Parse_error of string

(* A cell contains a digit 1–9 or 0 for empty *)
type cell = int

(* A Sudoku board is a 9×9 matrix of cells *)
type grid = cell array array

(* A position on the board *)
type position = {
  row : int;
  col : int;
}

(* Difficulty levels for generated puzzles *)
type difficulty =
  | Easy
  | Medium
  | Hard

let parse_error fmt = Printf.ksprintf (fun msg -> raise (Parse_error msg)) fmt

let parse_cell row col = function
  | `Int n when 0 <= n && n <= 9 -> n
  | _ ->
      parse_error "Invalid cell at row %d, col %d. Expected integer 0–9."
        (row + 1) (col + 1)

let parse_row row_index = function
  | `List cells as row ->
      let cells_list = Yojson.Basic.Util.to_list row in
      let count = List.length cells_list in
      if count <> 9 then
        parse_error "Row %d has %d cells; each row must contain 9 cells."
          (row_index + 1) count;
      cells_list |> List.mapi (parse_cell row_index) |> Array.of_list
  | _ -> parse_error "Row %d must be a list of integers." (row_index + 1)

let parse_board = function
  | `List rows as json ->
      let rows_list = Yojson.Basic.Util.to_list json in
      let row_count = List.length rows_list in
      if row_count <> 9 then
        parse_error "Board has %d rows; expected 9 rows." row_count;
      rows_list |> List.mapi parse_row |> Array.of_list
  | _ ->
      parse_error
        "Board must be a list of 9 rows, each containing 9 integers (0–9)."

let get_board_json json =
  match json with
  | `List _ -> json
  | `Assoc fields -> (
      match List.assoc_opt "board" fields with
      | Some board -> board
      | None ->
          parse_error
            "JSON object must contain a \"board\" field holding a 9×9 array of \
             numbers.")
  | _ ->
      parse_error
        "Expected a top-level list or an object with a \"board\" field."

let load_grid path =
  try
    let json = Yojson.Basic.from_file path in
    json |> get_board_json |> parse_board
  with
  | Parse_error _ as ex -> raise ex
  | Yojson.Json_error msg -> raise (Parse_error ("Malformed JSON: " ^ msg))
  | Sys_error msg -> raise (Parse_error msg)

let string_of_cell = function
  | 0 -> "."
  | n -> string_of_int n

let format_grid ?colorize grid =
  let colorize =
    match colorize with Some f -> f | None -> fun _ _ text -> text
  in
  let buf = Buffer.create 256 in
  let header = "     1 2 3   4 5 6   7 8 9\n" in
  let separator = "   +-------+-------+-------+\n" in
  Buffer.add_string buf header;
  let add_separator () = Buffer.add_string buf separator in
  let add_row r row =
    if r mod 3 = 0 then add_separator ();
    Buffer.add_string buf (Printf.sprintf "%2d | " (r + 1));
    Array.iteri
      (fun c cell ->
        Buffer.add_string buf (colorize r c (string_of_cell cell));
        Buffer.add_string buf " ";
        if (c + 1) mod 3 = 0 then Buffer.add_string buf "| ")
      row;
    Buffer.add_char buf '\n'
  in
  Array.iteri add_row grid;
  add_separator ();
  Buffer.contents buf

let print_grid ?colorize grid = format_grid ?colorize grid |> print_string

(** [contains_all_digits_1_to_9 arr] checks if array [arr] contains exactly one
    of each digit from 1 to 9 (no duplicates, no missing digits). *)
let contains_all_digits_1_to_9 arr =
  if Array.length arr <> 9 then false
  else
    let seen = Array.make 10 false in
    let rec check i =
      if i >= Array.length arr then
        (* Check that all digits 1-9 were seen *)
        let rec verify_digit d =
          if d > 9 then true
          else if not seen.(d) then false
          else verify_digit (d + 1)
        in
        verify_digit 1
      else
        let value = arr.(i) in
        (* Value must be in range 1-9 *)
        if value < 1 || value > 9 then false
        else if seen.(value) then false (* Duplicate found *)
        else (
          seen.(value) <- true;
          check (i + 1))
    in
    check 0

(** [is_row_valid grid row] checks if row [row] contains digits 1-9 exactly
    once. *)
let is_row_valid grid row = contains_all_digits_1_to_9 grid.(row)

(** [is_col_valid grid col] checks if column [col] contains digits 1-9 exactly
    once. *)
let is_col_valid grid col =
  let col_array = Array.init 9 (fun i -> grid.(i).(col)) in
  contains_all_digits_1_to_9 col_array

(** [is_box_valid grid box_row box_col] checks if the 3x3 box starting at
    [box_row] [box_col] contains digits 1-9 exactly once. *)
let is_box_valid grid box_row box_col =
  let box_array = Array.make 9 0 in
  let idx = ref 0 in
  for r = 0 to 2 do
    for c = 0 to 2 do
      box_array.(!idx) <- grid.((box_row * 3) + r).((box_col * 3) + c);
      idx := !idx + 1
    done
  done;
  contains_all_digits_1_to_9 box_array

(** [is_complete grid] checks if all cells in [grid] are filled (no zeros). *)
let is_complete grid =
  let rec check_row row =
    if row >= 9 then true
    else
      let rec check_col col =
        if col >= 9 then check_row (row + 1)
        else if grid.(row).(col) = 0 then false
        else check_col (col + 1)
      in
      check_col 0
  in
  check_row 0

(** [is_valid_sudoku grid] checks if [grid] is a valid completed Sudoku
    solution. Returns true if:
    - All cells are filled (no zeros)
    - Each row contains digits 1-9 exactly once (validates range 1-9)
    - Each column contains digits 1-9 exactly once (validates range 1-9)
    - Each 3x3 box contains digits 1-9 exactly once (validates range 1-9) *)
let is_valid_sudoku grid =
  if not (is_complete grid) then false
  else
    let rows_valid =
      let rec check_row i =
        if i >= 9 then true
        else if not (is_row_valid grid i) then false
        else check_row (i + 1)
      in
      check_row 0
    in
    if not rows_valid then false
    else
      let cols_valid =
        let rec check_col i =
          if i >= 9 then true
          else if not (is_col_valid grid i) then false
          else check_col (i + 1)
        in
        check_col 0
      in
      if not cols_valid then false
      else
        let boxes_valid =
          let rec check_box_row br =
            if br >= 3 then true
            else
              let rec check_box_col bc =
                if bc >= 3 then check_box_row (br + 1)
                else if not (is_box_valid grid br bc) then false
                else check_box_col (bc + 1)
              in
              check_box_col 0
          in
          check_box_row 0
        in
        boxes_valid

(** [is_cell_locked original_grid row col] checks if a cell was originally
    filled in the initial board. *)
let is_cell_locked original_grid row col = original_grid.(row).(col) <> 0

(** [is_consistent grid] checks that the current (possibly incomplete) grid has
    no duplicate non-zero values in any row, column, or 3x3 box. This is used as
    a quick fail-fast before attempting to solve. *)
let is_consistent grid =
  let check_unit cells =
    let seen = Array.make 10 false in
    let rec loop i =
      if i = Array.length cells then true
      else
        let v = cells.(i) in
        if v = 0 then loop (i + 1)
        else if v < 1 || v > 9 then false
        else if seen.(v) then false
        else (
          seen.(v) <- true;
          loop (i + 1))
    in
    loop 0
  in
  let rows_ok =
    let rec check_row r =
      if r = 9 then true
      else if check_unit grid.(r) then check_row (r + 1)
      else false
    in
    check_row 0
  in
  if not rows_ok then false
  else
    let cols_ok =
      let rec check_col c =
        if c = 9 then true
        else
          let col = Array.init 9 (fun r -> grid.(r).(c)) in
          if check_unit col then check_col (c + 1) else false
      in
      check_col 0
    in
    if not cols_ok then false
    else
      let boxes_ok =
        let rec check_box br bc =
          if br = 3 then true
          else if bc = 3 then check_box (br + 1) 0
          else
            let box =
              Array.init 9 (fun i ->
                  let r = (br * 3) + (i / 3) in
                  let c = (bc * 3) + (i mod 3) in
                  grid.(r).(c))
            in
            if check_unit box then check_box br (bc + 1) else false
        in
        check_box 0 0
      in
      boxes_ok

(** [would_create_duplicate grid row col value] checks if placing [value] at
    [row] [col] would create a duplicate in the row, column, or box. Returns
    true if it would create a duplicate, false otherwise. Value 0 (clearing) is
    always allowed. *)
let would_create_duplicate grid row col value =
  if value = 0 then false (* Clearing is always allowed *)
  else
    (* Check row for duplicates (excluding current cell) *)
    let row_has_duplicate =
      let rec check_col c =
        if c >= 9 then false
        else if c = col then check_col (c + 1)
        else if grid.(row).(c) = value then true
        else check_col (c + 1)
      in
      check_col 0
    in
    if row_has_duplicate then true
    else
      (* Check column for duplicates (excluding current cell) *)
      let col_has_duplicate =
        let rec check_row r =
          if r >= 9 then false
          else if r = row then check_row (r + 1)
          else if grid.(r).(col) = value then true
          else check_row (r + 1)
        in
        check_row 0
      in
      if col_has_duplicate then true
      else
        (* Check box for duplicates (excluding current cell) *)
        let box_row = row / 3 in
        let box_col = col / 3 in
        let box_has_duplicate =
          let rec check_box r =
            if r > 2 then false
            else
              let rec check_box_col c =
                if c > 2 then check_box (r + 1)
                else
                  let actual_row = (box_row * 3) + r in
                  let actual_col = (box_col * 3) + c in
                  if actual_row = row && actual_col = col then
                    check_box_col (c + 1)
                  else if grid.(actual_row).(actual_col) = value then true
                  else check_box_col (c + 1)
              in
              check_box_col 0
          in
          check_box 0
        in
        box_has_duplicate

(** [update_cell grid original_grid row col value] updates the cell at [row]
    [col] to [value]. Returns a new grid with the updated cell. Allows editing
    user-filled cells but prevents editing original (locked) cells. Validates
    that the move doesn't create duplicates. *)
let update_cell grid original_grid row col value =
  if row < 0 || row >= 9 || col < 0 || col >= 9 then
    raise
      (Parse_error
         (Printf.sprintf "Invalid coordinates: row %d, col %d (must be 1-9)"
            (row + 1) (col + 1)));
  if value < 0 || value > 9 then
    raise (Parse_error (Printf.sprintf "Invalid value: %d (must be 0-9)" value));
  (* Check if cell is locked (was originally filled) *)
  if is_cell_locked original_grid row col then
    raise
      (Parse_error
         (Printf.sprintf
            "Cell at (%d, %d) is part of the original puzzle and cannot be \
             changed."
            (col + 1) (row + 1)));
  (* Check if the move would create a duplicate *)
  (if would_create_duplicate grid row col value then
     (* Determine which constraint is violated for error message *)
     let error_msg =
       (* Check row for duplicates *)
       let rec check_row_col c =
         if c >= 9 then None
         else if c <> col && grid.(row).(c) = value then
           Some
             (Printf.sprintf
                "Duplicate %d in row %d (already exists at column %d)" value
                (row + 1) (c + 1))
         else check_row_col (c + 1)
       in
       match check_row_col 0 with
       | Some msg -> msg
       | None -> (
           (* Check column for duplicates *)
           let rec check_col_row r =
             if r >= 9 then None
             else if r <> row && grid.(r).(col) = value then
               Some
                 (Printf.sprintf
                    "Duplicate %d in column %d (already exists at row %d)" value
                    (col + 1) (r + 1))
             else check_col_row (r + 1)
           in
           match check_col_row 0 with
           | Some msg -> msg
           | None -> (
               (* Check box for duplicates *)
               let box_row = row / 3 in
               let box_col = col / 3 in
               let rec check_box_row r =
                 if r > 2 then None
                 else
                   let rec check_box_col c =
                     if c > 2 then check_box_row (r + 1)
                     else
                       let actual_row = (box_row * 3) + r in
                       let actual_col = (box_col * 3) + c in
                       if actual_row = row && actual_col = col then
                         check_box_col (c + 1)
                       else if grid.(actual_row).(actual_col) = value then
                         Some
                           (Printf.sprintf
                              "Duplicate %d in 3x3 box (already exists at row \
                               %d, column %d)"
                              value (actual_row + 1) (actual_col + 1))
                       else check_box_col (c + 1)
                   in
                   check_box_col 0
               in
               match check_box_row 0 with
               | Some msg -> msg
               | None -> Printf.sprintf "Duplicate %d detected" value))
     in
     raise
       (Parse_error
          (Printf.sprintf "Cannot place %d at (%d, %d). %s" value (col + 1)
             (row + 1) error_msg)));
  let new_grid = Array.map Array.copy grid in
  new_grid.(row).(col) <- value;
  new_grid

(* Initialize the PRNG once for generator usage *)
let () = Random.self_init ()

let copy_grid grid = Array.map Array.copy grid

let shuffle_array arr =
  let a = Array.copy arr in
  for i = Array.length a - 1 downto 1 do
    let j = Random.int (i + 1) in
    let tmp = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- tmp
  done;
  a

(* Generate a fully-solved grid using randomized backtracking *)
let generate_complete_solution () =
  let grid = Array.make_matrix 9 9 0 in
  let positions =
    Array.init 81 (fun i -> (i / 9, i mod 9)) |> shuffle_array
  in
  let rec fill idx =
    if idx = Array.length positions then true
    else
      let r, c = positions.(idx) in
      let numbers = shuffle_array [| 1; 2; 3; 4; 5; 6; 7; 8; 9 |] in
      let rec try_number i =
        if i = Array.length numbers then (
          grid.(r).(c) <- 0;
          false)
        else
          let n = numbers.(i) in
          if would_create_duplicate grid r c n then try_number (i + 1)
          else (
            grid.(r).(c) <- n;
            if fill (idx + 1) then true
            else try_number (i + 1))
      in
      try_number 0
  in
  if fill 0 then grid
  else raise (Failure "Failed to generate a complete Sudoku solution.")

let clues_for_difficulty = function
  | Easy -> 40
  | Medium -> 32
  | Hard -> 25

let generate difficulty =
  let solution = generate_complete_solution () in
  let puzzle = copy_grid solution in
  let removals = max 0 (81 - clues_for_difficulty difficulty) in
  let cells =
    Array.init 81 (fun i -> (i / 9, i mod 9)) |> shuffle_array
  in
  for i = 0 to removals - 1 do
    let r, c = cells.(i) in
    puzzle.(r).(c) <- 0
  done;
  puzzle

(** [solve grid] attempts to solve a Sudoku puzzle using backtracking. Returns
    [Ok solved_grid] if a solution is found. If the puzzle has internal
    conflicts or no solution exists, returns [Error msg]. The input [grid] is
    never mutated. *)
let solve grid =
  (* Fail fast if the starting grid is inconsistent *)
  if not (is_consistent grid) then
    Error "Unsolvable: the initial board contains conflicts."
  else
    let working = Array.map Array.copy grid in
    let rec find_empty r c =
      if r = 9 then None
      else if c = 9 then find_empty (r + 1) 0
      else if working.(r).(c) = 0 then Some (r, c)
      else find_empty r (c + 1)
    in
    let rec try_value r c value =
      if value > 9 then false
      else if would_create_duplicate working r c value then
        try_value r c (value + 1)
      else (
        working.(r).(c) <- value;
        if backtrack () then true
        else (
          working.(r).(c) <- 0;
          try_value r c (value + 1)))
    and backtrack () =
      match find_empty 0 0 with
      | None -> true
      | Some (r, c) -> try_value r c 1
    in
    if backtrack () then Ok working
    else Error "Unsolvable: no valid solution exists for this board."

(** Fisher–Yates shuffle of digits 1-9 to add randomness to generation. *)
let shuffled_digits rng =
  let arr = [| 1; 2; 3; 4; 5; 6; 7; 8; 9 |] in
  for i = Array.length arr - 1 downto 1 do
    let j = Random.State.int rng (i + 1) in
    let tmp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- tmp
  done;
  arr

(** [generate_full_grid ?rng ()] constructs a fully solved Sudoku board by
    filling an empty grid using backtracking with randomized digit order. *)
let generate_full_grid ?rng () =
  let rng = match rng with Some r -> r | None -> Random.State.make_self_init () in
  let grid = Array.make_matrix 9 9 0 in
  let rec find_empty r c =
    if r = 9 then None
    else if c = 9 then find_empty (r + 1) 0
    else if grid.(r).(c) = 0 then Some (r, c)
    else find_empty r (c + 1)
  in
  let rec try_values r c digits idx =
    if idx = Array.length digits then false
    else
      let value = digits.(idx) in
      if would_create_duplicate grid r c value then
        try_values r c digits (idx + 1)
      else (
        grid.(r).(c) <- value;
        if backtrack () then true
        else (
          grid.(r).(c) <- 0;
          try_values r c digits (idx + 1)))
  and backtrack () =
    match find_empty 0 0 with
    | None -> true
    | Some (r, c) ->
        let digits = shuffled_digits rng in
        try_values r c digits 0
  in
  if backtrack () then Ok grid
  else Error "Generation failed: could not construct a full grid."

let shuffled_positions rng =
  let arr = Array.init 81 (fun i -> (i / 9, i mod 9)) in
  for i = Array.length arr - 1 downto 1 do
    let j = Random.State.int rng (i + 1) in
    let tmp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- tmp
  done;
  arr

let generate_puzzle ?rng ?(clues = 35) () =
  if clues < 17 || clues > 81 then
    Error "Clue count must be between 17 and 81 for a standard 9x9 Sudoku."
  else
    let rng = match rng with Some r -> r | None -> Random.State.make_self_init () in
    match generate_full_grid ~rng () with
    | Error msg -> Error msg
    | Ok solution ->
        let puzzle = Array.map Array.copy solution in
        let positions = shuffled_positions rng in
        let remaining = ref 81 in
        let rec remove idx =
          if !remaining <= clues then Ok puzzle
          else if idx >= Array.length positions then Ok puzzle
          else
            let r, c = positions.(idx) in
            let original = puzzle.(r).(c) in
            puzzle.(r).(c) <- 0;
            decr remaining;
            (* Ensure the puzzle is still solvable *)
            match solve puzzle with
            | Ok _ -> remove (idx + 1)
            | Error _ ->
                puzzle.(r).(c) <- original;
                incr remaining;
                remove (idx + 1)
        in
        remove 0
