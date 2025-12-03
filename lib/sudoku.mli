(** Types and helpers for loading and printing a Sudoku board. *)

exception Parse_error of string

type cell = int
type grid = cell array array

type position = {
  row : int;
  col : int;
}

type difficulty =
  | Easy
  | Medium
  | Hard

val load_grid : string -> grid
(** [load_grid path] parses a Sudoku board from the JSON file at [path]. The
    JSON is expected to be either a 9×9 array or an object with a ["board"]
    field containing that array. Cells may be integers 0–9, where 0 represents
    an empty cell. *)

val format_grid :
  ?colorize:(int -> int -> string -> string) -> grid -> string
(** Render a grid as a string using ASCII separators. *)

val print_grid : ?colorize:(int -> int -> string -> string) -> grid -> unit
(** Print a grid to stdout. *)

val generate : difficulty -> grid
(** [generate difficulty] creates a new Sudoku puzzle as a 9×9 array. The board
    is derived from a randomly generated solved grid with cells removed based on
    [difficulty] (easy keeps the most clues, hard the fewest). *)

val update_cell : grid -> grid -> int -> int -> int -> grid
(** [update_cell grid original_grid row col value] updates the cell at [row]
    [col] (0-indexed) to [value] (0-9). Returns a new grid with the updated
    cell. Allows editing user-filled cells but prevents editing original
    (locked) cells. Raises [Parse_error] if coordinates or value are invalid, or
    if attempting to edit a locked cell. *)

val solve : grid -> (grid, string) result
(** [solve grid] attempts to solve a Sudoku puzzle using backtracking. Returns
    [Ok solution] if the puzzle can be solved, or [Error msg] if the board has
    conflicts or no solution exists. The input grid is left unchanged. *)

val is_valid_sudoku : grid -> bool
(** [is_valid_sudoku grid] checks if [grid] is a valid completed Sudoku
    solution. Returns true if:
    - All cells are filled (no zeros)
    - Each row contains digits 1-9 exactly once (validates range 1-9)
    - Each column contains digits 1-9 exactly once (validates range 1-9)
    - Each 3x3 box contains digits 1-9 exactly once (validates range 1-9) *)

val is_complete : grid -> bool
(** [is_complete grid] checks if all cells in [grid] are filled (no zeros). *)
