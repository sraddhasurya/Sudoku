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

let format_grid grid =
  let buf = Buffer.create 256 in
  let header = "    1 2 3   4 5 6   7 8 9\n" in
  let separator = "  +-------+-------+-------+\n" in
  Buffer.add_string buf header;
  let add_separator () = Buffer.add_string buf separator in
  let add_row r row =
    if r mod 3 = 0 then add_separator ();
    Buffer.add_string buf (Printf.sprintf "%2d | " (r + 1));
    Array.iteri
      (fun c cell ->
        Buffer.add_string buf (string_of_cell cell);
        Buffer.add_string buf " ";
        if (c + 1) mod 3 = 0 then Buffer.add_string buf "| ")
      row;
    Buffer.add_char buf '\n'
  in
  Array.iteri add_row grid;
  add_separator ();
  Buffer.contents buf

let print_grid grid = format_grid grid |> print_string

(** [update_cell grid row col value] updates the cell at [row] [col] to [value].
    Returns a new grid with the updated cell. *)
let update_cell grid row col value =
  if row < 0 || row >= 9 || col < 0 || col >= 9 then
    raise
      (Parse_error
         (Printf.sprintf "Invalid coordinates: row %d, col %d (must be 1-9)"
            (row + 1) (col + 1)));
  if value < 0 || value > 9 then
    raise (Parse_error (Printf.sprintf "Invalid value: %d (must be 0-9)" value));
  (* Check if cell is already filled *)
  if grid.(row).(col) <> 0 then
    raise
      (Parse_error
         (Printf.sprintf
            "Cell at (%d, %d) already contains %d. Please pick a different \
             cell."
            (col + 1) (row + 1)
            grid.(row).(col)));
  let new_grid = Array.map Array.copy grid in
  new_grid.(row).(col) <- value;
  new_grid
