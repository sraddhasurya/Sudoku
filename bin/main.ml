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

let rec interactive_loop grid original_grid =
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
          let updated_grid =
            Sudoku.update_cell grid original_grid row col value
          in
          print_endline "";
          Sudoku.print_grid updated_grid;
          (* Check if the game is complete and valid *)
          if Sudoku.is_complete updated_grid then
            if Sudoku.is_valid_sudoku updated_grid then (
              print_endline
                "\n Congratulations! You solved the Sudoku puzzle correctly!";
              exit 0)
            else (
              print_endline
                "\n\
                 The board is complete, but it's not a valid Sudoku solution. \
                 Please check for duplicates in rows, columns, or boxes.";
              interactive_loop updated_grid original_grid)
          else interactive_loop updated_grid original_grid
        with Sudoku.Parse_error msg ->
          prerr_endline ("Error: " ^ msg);
          interactive_loop grid original_grid)
  with
  | End_of_file ->
      print_endline "\nGoodbye!";
      exit 0
  | Invalid_argument msg ->
      prerr_endline ("Error: " ^ msg);
      interactive_loop grid original_grid

let () =
  match Array.to_list Sys.argv |> List.tl with
  | [ path ] -> (
      try
        let original_grid = path |> Sudoku.load_grid in
        (* Create a copy for the current game state *)
        let grid = Array.map Array.copy original_grid in
        Sudoku.print_grid grid;
        interactive_loop grid original_grid
      with Sudoku.Parse_error msg ->
        prerr_endline ("Error: " ^ msg);
        exit 1)
  | _ -> usage ()
