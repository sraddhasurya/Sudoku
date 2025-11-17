let usage () =
  prerr_endline "Usage: sudoku <path-to-board.json>";
  exit 1

(* Parse user input in format: <number> (<x_coordinate>, <y_coordinate>) *)
let parse_input input =
  let input = String.trim input in
  (* Check for exit commands *)
  if input = "quit" || input = "exit" || input = "q" then None
  else
    try
      (* Match pattern: number (x, y) *)
      (* In Str.regexp, \( and \) are capturing groups, literal parens are just ( and ) *)
      let regexp =
        Str.regexp
          "^\\([0-9]\\)[ \t]*([ \t]*\\([0-9]\\)[ \t]*,[ \t]*\\([0-9]\\)[ \t]*)$"
      in
      if Str.string_match regexp input 0 then
        let value = int_of_string (Str.matched_group 1 input) in
        let x = int_of_string (Str.matched_group 2 input) in
        let y = int_of_string (Str.matched_group 3 input) in
        Some (value, x, y)
      else raise (Invalid_argument "Invalid format")
    with _ ->
      raise
        (Invalid_argument
           "Invalid format. Use: <number> (<x>, <y>) or 'quit' to exit")

let rec interactive_loop grid =
  print_string "\nEnter move (format: <number> (<x>, <y>)) or 'quit' to exit: ";
  flush stdout;
  try
    let input = read_line () in
    match parse_input input with
    | None ->
        (* quit command *)
        print_endline "Goodbye!";
        exit 0
    | Some (value, x, y) -> (
        (* Convert 1-indexed coordinates to 0-indexed *)
        let row = y - 1 in
        let col = x - 1 in
        try
          let updated_grid = Sudoku.update_cell grid row col value in
          print_endline "";
          Sudoku.print_grid updated_grid;
          interactive_loop updated_grid
        with Sudoku.Parse_error msg ->
          prerr_endline ("Error: " ^ msg);
          interactive_loop grid)
  with
  | End_of_file ->
      print_endline "\nGoodbye!";
      exit 0
  | Invalid_argument msg ->
      prerr_endline ("Error: " ^ msg);
      interactive_loop grid

let () =
  match Array.to_list Sys.argv |> List.tl with
  | [ path ] -> (
      try
        let grid = path |> Sudoku.load_grid in
        Sudoku.print_grid grid;
        interactive_loop grid
      with Sudoku.Parse_error msg ->
        prerr_endline ("Error: " ^ msg);
        exit 1)
  | _ -> usage ()
