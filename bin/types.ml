(* A cell contains a digit 1–9 or 0 for empty *)
type cell = int

(* A Sudoku board is a 9×9 matrix of cells *)
type grid = cell array array

(* A position on the board *)
type position = {
  row : int;
  (* 0–8 *) col : int; (* 0–8 *)
}
