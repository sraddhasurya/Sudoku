(** Types and helpers for loading and printing a Sudoku board. *)

exception Parse_error of string

type cell = int
type grid = cell array array
type position = { row : int; col : int }

(** [load_grid path] parses a Sudoku board from the JSON file at [path].
    The JSON is expected to be either a 9×9 array or an object with a
    ["board"] field containing that array. Cells may be integers 0–9,
    where 0 represents an empty cell. *)
val load_grid : string -> grid

(** Render a grid as a string using ASCII separators. *)
val format_grid : grid -> string

(** Print a grid to stdout. *)
val print_grid : grid -> unit
