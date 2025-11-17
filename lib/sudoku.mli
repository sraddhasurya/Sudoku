(** Types and helpers for loading and printing a Sudoku board. *)

exception Parse_error of string

type cell = int
type grid = cell array array

type position = {
  row : int;
  col : int;
}

val load_grid : string -> grid
(** [load_grid path] parses a Sudoku board from the JSON file at [path]. The
    JSON is expected to be either a 9×9 array or an object with a ["board"]
    field containing that array. Cells may be integers 0–9, where 0 represents
    an empty cell. *)

val format_grid : grid -> string
(** Render a grid as a string using ASCII separators. *)

val print_grid : grid -> unit
(** Print a grid to stdout. *)

val update_cell : grid -> grid -> int -> int -> int -> grid
(** [update_cell grid original_grid row col value] updates the cell at [row]
    [col] (0-indexed) to [value] (0-9). Returns a new grid with the updated
    cell. Allows editing user-filled cells but prevents editing original
    (locked) cells. Raises [Parse_error] if coordinates or value are invalid, or
    if attempting to edit a locked cell. *)

val is_valid_sudoku : grid -> bool
(** [is_valid_sudoku grid] checks if [grid] is a valid completed Sudoku
    solution. Returns true if:
    - All cells are filled (no zeros)
    - Each row contains digits 1-9 exactly once (validates range 1-9)
    - Each column contains digits 1-9 exactly once (validates range 1-9)
    - Each 3x3 box contains digits 1-9 exactly once (validates range 1-9) *)

val is_complete : grid -> bool
(** [is_complete grid] checks if all cells in [grid] are filled (no zeros). *)
