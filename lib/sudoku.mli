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

val update_cell : grid -> int -> int -> int -> grid
(** [update_cell grid row col value] updates the cell at [row] [col] (0-indexed)
    to [value] (0-9). Returns a new grid with the updated cell. Raises
    [Parse_error] if coordinates or value are invalid. *)
