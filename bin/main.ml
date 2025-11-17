let usage () =
  prerr_endline "Usage: sudoku <path-to-board.json>";
  exit 1

let () =
  match Array.to_list Sys.argv |> List.tl with
  | [path] -> (
      try
        path |> Sudoku.load_grid |> Sudoku.print_grid
      with
      | Sudoku.Parse_error msg ->
          prerr_endline ("Error: " ^ msg);
          exit 1)
  | _ -> usage ()
