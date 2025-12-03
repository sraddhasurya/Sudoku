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

let rec prompt_autocorrect () =
  print_string "Enable autocorrect mode? (y/n): ";
  flush stdout;
  match String.lowercase_ascii (String.trim (read_line ())) with
  | "y" | "yes" -> true
  | "n" | "no" -> false
  | _ ->
      prerr_endline "Please answer y or n.";
      prompt_autocorrect ()

let colorize_incorrect incorrect =
  fun r c text ->
    if incorrect.(r).(c) then "\027[31m" ^ text ^ "\027[0m" else text

let print_board ~autocorrect incorrect grid =
  if autocorrect then
    Sudoku.print_grid ~colorize:(colorize_incorrect incorrect) grid
  else Sudoku.print_grid grid

let update_incorrect incorrect solution row col value =
  let next = Array.map Array.copy incorrect in
  next.(row).(col) <- value <> 0 && value <> solution.(row).(col);
  next

let rec interactive_loop grid original_grid autocorrect solution incorrect =
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
          let incorrect' =
            match (autocorrect, solution) with
            | true, Some sol -> update_incorrect incorrect sol row col value
            | _ -> incorrect
          in
          print_endline "";
          print_board ~autocorrect incorrect' updated_grid;
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
              interactive_loop updated_grid original_grid autocorrect solution
                incorrect')
          else
            interactive_loop updated_grid original_grid autocorrect solution
              incorrect'
        with Sudoku.Parse_error msg ->
          prerr_endline ("Error: " ^ msg);
          interactive_loop grid original_grid autocorrect solution incorrect)
  with
  | End_of_file ->
      print_endline "\nGoodbye!";
      exit 0
  | Invalid_argument msg ->
      prerr_endline ("Error: " ^ msg);
      interactive_loop grid original_grid autocorrect solution incorrect

let start_game initial_grid =
  let original_grid = Array.map Array.copy initial_grid in
  let grid = Array.map Array.copy initial_grid in
  let solution_opt =
    match Sudoku.solve original_grid with
    | Ok sol -> Some sol
    | Error msg ->
        prerr_endline
          ("Autocorrect unavailable because the solver could not find a \
            solution: " ^ msg);
        None
  in
  let autocorrect =
    let wants_autocorrect = prompt_autocorrect () in
    match (wants_autocorrect, solution_opt) with
    | true, Some _ -> true
    | true, None ->
        prerr_endline "Starting without autocorrect.";
        false
    | false, _ -> false
  in
  let incorrect = Array.make_matrix 9 9 false in
  print_board ~autocorrect incorrect grid;
  interactive_loop grid original_grid autocorrect solution_opt incorrect

let () =
  match Array.to_list Sys.argv |> List.tl with
  | [] -> usage ()
  | [ path ] -> (
      try
        let original_grid = path |> Sudoku.load_grid in
        start_game original_grid
      with Sudoku.Parse_error msg ->
        prerr_endline ("Error: " ^ msg);
        exit 1)
  | _ -> usage ()
