let usage () =
  prerr_endline "Usage: sudoku <path-to-board.json>";
  exit 1

let format_elapsed start_time =
  let elapsed = Unix.gettimeofday () -. start_time in
  let minutes = int_of_float (elapsed /. 60.) in
  let seconds = int_of_float (elapsed -. float_of_int (minutes * 60)) in
  Printf.sprintf "%d:%02d" minutes seconds

(* Parse user input in format: <number> (<row>, <col>) *)
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
        let row = int_of_string (Str.matched_group 2 input) in
        let col = int_of_string (Str.matched_group 3 input) in
        Some (value, row, col)
      else raise (Invalid_argument "Invalid format")
    with _ ->
      raise
        (Invalid_argument
           "Invalid format. Use: <number> (<row>, <col>) or 'quit' to exit or \
            'clear' to clear board")

let rec prompt_autocorrect () =
  print_string "Enable autocorrect mode? (y/n): ";
  flush stdout;
  match read_line () with
  | exception End_of_file ->
      prerr_endline "\nNo input detected. Starting without autocorrect.";
      false
  | line -> (
      match String.lowercase_ascii (String.trim line) with
      | "y" | "yes" -> true
      | "n" | "no" -> false
      | _ ->
          prerr_endline "Please answer y or n.";
          prompt_autocorrect ())

let colorize_board grid incorrect =
  let red text = "\027[31m" ^ text ^ "\027[0m" in
  fun r c text -> if incorrect.(r).(c) then red text else text

let print_board ~autocorrect incorrect grid =
  if autocorrect then
    Sudoku.print_grid ~colorize:(colorize_board grid incorrect) grid
  else Sudoku.print_grid grid

let update_incorrect incorrect solution row col value =
  let next = Array.map Array.copy incorrect in
  next.(row).(col) <- value <> 0 && value <> solution.(row).(col);
  next

let rec handle_completion grid original_grid autocorrect solution incorrect
    mistakes start_time =
  if Sudoku.is_complete grid then
    if Sudoku.is_valid_sudoku grid then (
      print_endline
        "\n Congratulations! You solved the Sudoku puzzle correctly!";
      Printf.printf "Time: %s\n%!" (format_elapsed start_time);
      exit 0)
    else (
      print_endline
        "\n\
         The board is complete, but it's not a valid Sudoku solution. Please \
         check for duplicates in rows, columns, or boxes.";
      interactive_loop grid original_grid autocorrect solution incorrect
        mistakes start_time)
  else
    interactive_loop grid original_grid autocorrect solution incorrect mistakes
      start_time

and interactive_loop grid original_grid autocorrect solution incorrect mistakes
    start_time =
  print_endline (Printf.sprintf "\nMistakes: %d/3" mistakes);
  print_string "Enter move: ";
  flush stdout;
  try
    let input = read_line () |> String.trim in
    let lower = String.lowercase_ascii input in
    if lower = "clear" then (
      let reset_grid = Array.map Array.copy original_grid in
      let incorrect_reset = Array.make_matrix 9 9 false in
      print_endline "\nBoard reset to original puzzle.";
      print_board ~autocorrect incorrect_reset reset_grid;
      interactive_loop reset_grid original_grid autocorrect solution
        incorrect_reset mistakes start_time)
    else
      match parse_input input with
    | None ->
        (* quit command *)
        print_endline "Goodbye!";
        Printf.printf "Time: %s\n%!" (format_elapsed start_time);
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
          let mistake_inc =
            match solution with
            | Some sol -> value <> 0 && value <> sol.(row).(col)
            | None -> false
          in
          let mistakes' = mistakes + (if mistake_inc then 1 else 0) in
          print_endline "";
          print_board ~autocorrect incorrect' updated_grid;
          if mistakes' >= 3 then (
            print_endline
              "\nYou ran out of tries. The correct board is:\n";
            (match solution with
            | Some sol -> Sudoku.print_grid sol
            | None ->
                print_endline
                  "Solver could not produce a solution to display.");
            Printf.printf "Time: %s\n%!" (format_elapsed start_time);
            exit 0);
          (* Check if the game is complete and valid *)
          if Sudoku.is_complete updated_grid then
            if Sudoku.is_valid_sudoku updated_grid then (
              print_endline
                "\n Congratulations! You solved the Sudoku puzzle correctly!";
              Printf.printf "Time: %s\n%!" (format_elapsed start_time);
              exit 0)
            else (
              print_endline
                "\n\
                 The board is complete, but it's not a valid Sudoku solution. \
                 Please check for duplicates in rows, columns, or boxes.";
              interactive_loop updated_grid original_grid autocorrect solution
                incorrect' mistakes' start_time)
          else
            interactive_loop updated_grid original_grid autocorrect solution
              incorrect' mistakes' start_time
        with Sudoku.Parse_error msg ->
          prerr_endline ("Error: " ^ msg);
          interactive_loop grid original_grid autocorrect solution incorrect
            mistakes start_time)
  with
  | End_of_file ->
      print_endline "\nGoodbye!";
      Printf.printf "Time: %s\n%!" (format_elapsed start_time);
      exit 0
  | Invalid_argument msg ->
      prerr_endline ("Error: " ^ msg);
      interactive_loop grid original_grid autocorrect solution incorrect
        mistakes start_time

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
  print_endline
    "\n\
     Commands:\n\
     - Enter moves as: <number> (<row>, <col>)\n\
     - Type 'clear' to reset to the original puzzle\n\
     - Type 'quit' to exit\n\
     You have 3 mistakes total; after 3 wrong entries the solution is shown.";
  let incorrect = Array.make_matrix 9 9 false in
  let start_time = Unix.gettimeofday () in
  print_board ~autocorrect incorrect grid;
  handle_completion grid original_grid autocorrect solution_opt incorrect 0
    start_time

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
