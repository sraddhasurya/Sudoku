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
          "^\\(-?[0-9]+\\)[ \t]*([ \t]*\\([0-9]+\\)[ \t]*,[ \t]*\\([0-9]+\\)[ \t]*)$"
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
           "Invalid format. Use: <number> (<row>, <col>) or 'hint'/'quit' to \
            exit or 'clear' to clear board")

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

let colorize_board original_grid incorrect hint_pos =
  let red text = "\027[31m" ^ text ^ "\027[0m" in
  let green text = "\027[32m" ^ text ^ "\027[0m" in
  fun r c text ->
    (* Hint takes precedence *)
    (match hint_pos with
    | Some (hr, hc) when hr = r && hc = c -> green text
    | _ ->
        let base = if incorrect.(r).(c) then red text else text in
        if original_grid.(r).(c) <> 0 then "\027[1m" ^ base ^ "\027[0m" else base)

let print_board ?hint ~autocorrect original_grid incorrect grid =
  let hint_pos = match hint with None -> None | Some p -> Some p in
  let incorrect_to_show =
    if autocorrect then incorrect else Array.make_matrix 9 9 false
  in
  Sudoku.print_grid
    ~colorize:(colorize_board original_grid incorrect_to_show hint_pos)
    grid

let update_incorrect incorrect solution row col value =
  let next = Array.map Array.copy incorrect in
  next.(row).(col) <- value <> 0 && value <> solution.(row).(col);
  next

let duplicate_error msg =
  try
    ignore (Str.search_forward (Str.regexp_case_fold "duplicate") msg 0);
    true
  with Not_found -> false

type next_step =
  | Quit
  | Menu

let rec prompt_menu_or_quit () =
  print_string "Type 'menu' to choose another puzzle or 'quit' to exit: ";
  flush stdout;
  match read_line () with
  | exception End_of_file -> Quit
  | line -> (
      match String.lowercase_ascii (String.trim line) with
      | "menu" -> Menu
      | "quit" | "exit" | "q" -> Quit
      | _ ->
          print_endline "Please type 'menu' or 'quit'.";
          prompt_menu_or_quit ())

type action =
  | CmdClear
  | CmdQuit
  | CmdHint
  | CmdMenu
  | Move of int * int * int

let read_field label =
  print_string label;
  flush stdout;
  match read_line () with
  | exception End_of_file -> raise End_of_file
  | s -> String.trim s

let read_action () =
  print_string "Enter value row col (ex: 1 1 1) (or 'hint', 'clear', 'menu', 'quit'): ";
  flush stdout;
  match read_line () with
  | exception End_of_file -> raise End_of_file
  | line -> (
      let trimmed = String.trim line in
      match String.lowercase_ascii trimmed with
      | "hint" | "h" -> CmdHint
      | "clear" -> CmdClear
      | "menu" -> CmdMenu
      | "quit" | "exit" | "q" -> CmdQuit
      | _ -> (
          let parts =
            trimmed
            |> String.split_on_char ' '
            |> List.filter (fun s -> String.trim s <> "")
          in
          match parts with
          | [ v; r; c ] -> (
              try
                let value = int_of_string v in
                let row = int_of_string r in
                let col = int_of_string c in
                Move (value, row, col)
              with Failure _ ->
                raise
                  (Invalid_argument
                     "Please enter three numbers: value row col (or a \
                      command)."))
          | _ ->
              raise
                (Invalid_argument
                   "Please enter three numbers separated by spaces: value row \
                    col (or a command).")))

let rec handle_completion grid original_grid autocorrect solution incorrect
    hints mistakes start_time =
  if Sudoku.is_complete grid then
    if Sudoku.is_valid_sudoku grid then (
      print_endline
        "\n Congratulations! You solved the Sudoku puzzle correctly!";
      Printf.printf "Time: %s\n%!" (format_elapsed start_time);
      prompt_menu_or_quit ())
    else (
      print_endline
        "\n\
         The board is complete, but it's not a valid Sudoku solution. Please \
         check for duplicates in rows, columns, or boxes.";
      interactive_loop grid original_grid autocorrect solution incorrect hints
        mistakes start_time)
  else
    interactive_loop grid original_grid autocorrect solution incorrect hints
      mistakes start_time

and interactive_loop grid original_grid autocorrect solution incorrect hints
    mistakes start_time =
  if autocorrect then print_endline (Printf.sprintf "\nMistakes: %d/3" mistakes)
  else print_endline "";
  try
    match read_action () with
    | CmdClear ->
      let reset_grid = Array.map Array.copy original_grid in
      let incorrect_reset = Array.make_matrix 9 9 false in
      let hints_reset = Array.make_matrix 9 9 false in
      print_endline "\nBoard reset to original puzzle.";
      print_board ~autocorrect original_grid incorrect_reset reset_grid;
      interactive_loop reset_grid original_grid autocorrect solution
        incorrect_reset hints_reset mistakes start_time
    | CmdHint -> (
        match solution with
        | None ->
            prerr_endline "Hint unavailable: solver could not find a solution.";
            interactive_loop grid original_grid autocorrect solution incorrect
              hints mistakes start_time
        | Some sol -> (
            (* Collect positions that are empty in the current grid and in the
               original puzzle (so we don't overwrite original clues). *)
            let empties = ref [] in
            for r = 0 to 8 do
              for c = 0 to 8 do
                if grid.(r).(c) = 0 && original_grid.(r).(c) = 0 then
                  empties := (r, c) :: !empties
              done
            done;
            match !empties with
            | [] ->
                prerr_endline "No available empty cells for a hint.";
                interactive_loop grid original_grid autocorrect solution
                  incorrect hints mistakes start_time
            | _ ->
                let arr = Array.of_list !empties in
                let idx = Random.int (Array.length arr) in
                let hr, hc = arr.(idx) in
                (* Place the solved value into a copy of the current grid without
                   validating against the possibly-incorrect current entries. *)
                let new_grid = Array.map Array.copy grid in
                new_grid.(hr).(hc) <- sol.(hr).(hc);
                (* Optionally lock the hinted cell into original grid so it cannot be edited *)
                let new_original = Array.map Array.copy original_grid in
                new_original.(hr).(hc) <- sol.(hr).(hc);
                let hints' = Array.map Array.copy hints in
                hints'.(hr).(hc) <- true;
                print_endline "";
                print_board ~autocorrect ~hint:(hr, hc) new_original incorrect
                  new_grid;
                interactive_loop new_grid new_original autocorrect solution
                  incorrect hints' mistakes start_time))
    | CmdQuit ->
        print_endline "Goodbye!";
        Printf.printf "Time: %s\n%!" (format_elapsed start_time);
        Quit
    | CmdMenu ->
        print_endline "Returning to menu...";
        Menu
    | Move (value, row_input, col_input) -> (
        if value < 0 || value > 9 then (
          prerr_endline "Number must be between 1 and 9 (use 0 to clear).";
          interactive_loop grid original_grid autocorrect solution incorrect
            hints mistakes start_time)
        else if row_input < 1 || row_input > 9 || col_input < 1 || col_input > 9
        then (
          prerr_endline "Row and column numbers must be between 1 and 9.";
          interactive_loop grid original_grid autocorrect solution incorrect
            hints mistakes start_time)
        else
          (* Convert 1-indexed coordinates to 0-indexed *)
          let row = row_input - 1 in
          let col = col_input - 1 in
          if hints.(row).(col) then (
          prerr_endline "Cannot change a hinted cell.";
          interactive_loop grid original_grid autocorrect solution incorrect
            hints mistakes start_time)
          else
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
          print_board ~autocorrect original_grid incorrect' updated_grid;
          if mistakes' >= 3 then (
            print_endline
              "\nYou ran out of tries. The correct board is:\n";
            (match solution with
            | Some sol -> Sudoku.print_grid sol
            | None ->
                print_endline
                  "Solver could not produce a solution to display.");
            Printf.printf "Time: %s\n%!" (format_elapsed start_time);
            prompt_menu_or_quit ())
          else if Sudoku.is_complete updated_grid then
            if Sudoku.is_valid_sudoku updated_grid then (
              print_endline
                "\n Congratulations! You solved the Sudoku puzzle correctly!";
              Printf.printf "Time: %s\n%!" (format_elapsed start_time);
              prompt_menu_or_quit ())
            else (
              print_endline
                "\n\
                 The board is complete, but it's not a valid Sudoku solution. \
                 Please check for duplicates in rows, columns, or boxes.";
              interactive_loop updated_grid original_grid autocorrect solution
                incorrect' hints mistakes' start_time)
          else
            interactive_loop updated_grid original_grid autocorrect solution
              incorrect' hints mistakes' start_time
        with Sudoku.Parse_error msg ->
          if duplicate_error msg then (
            let mistakes' = mistakes + 1 in
            let updated_grid = Array.map Array.copy grid in
            updated_grid.(row).(col) <- value;
            let incorrect' =
              match (autocorrect, solution) with
              | true, Some sol -> update_incorrect incorrect sol row col value
              | _ -> incorrect
            in
            print_endline "";
            print_board ~autocorrect original_grid incorrect' updated_grid;
            if mistakes' >= 3 then (
              print_endline
                "\nYou ran out of tries. The correct board is:\n";
              (match solution with
              | Some sol -> Sudoku.print_grid sol
              | None ->
                  print_endline
                    "Solver could not produce a solution to display.");
              Printf.printf "Time: %s\n%!" (format_elapsed start_time);
              prompt_menu_or_quit ())
            else
              interactive_loop updated_grid original_grid autocorrect solution
                incorrect' hints mistakes' start_time)
          else (
            prerr_endline ("Error: " ^ msg);
            interactive_loop grid original_grid autocorrect solution incorrect
              hints mistakes start_time))
  with
  | End_of_file ->
      print_endline "\nGoodbye!";
      Printf.printf "Time: %s\n%!" (format_elapsed start_time);
      Quit
  | Invalid_argument msg ->
      prerr_endline ("Error: " ^ msg);
      interactive_loop grid original_grid autocorrect solution incorrect
        hints mistakes start_time

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
     - Enter moves as: value row col (1-9; 0 clears)\n\
     - Type 'hint' to fill a random empty cell correctly\n\
     - Type 'clear' to reset to the original puzzle\n\
     - Type 'menu' to pick a different puzzle\n\
     - Type 'quit' to exit\n\
     You have 3 mistakes total; after 3 wrong entries the solution is shown.";
  let incorrect = Array.make_matrix 9 9 false in
  let hints = Array.make_matrix 9 9 false in
  let start_time = Unix.gettimeofday () in
  print_board ~autocorrect original_grid incorrect grid;
  handle_completion grid original_grid autocorrect solution_opt incorrect hints
    0 start_time

let () =
  (* Allow optional path argument; if none provided, ask user for difficulty
     and pick a random board from the corresponding subfolder in
     ./boards/<difficulty>. Typing 'menu' during a game returns here to pick a
     new puzzle. *)
  let args = Array.to_list Sys.argv |> List.tl in
  Random.self_init ();
  let cwd = Sys.getcwd () in
  let boards_dir = Filename.concat cwd "boards" in

  let pick_random_board_for difficulty =
    let dir = Filename.concat boards_dir difficulty in
    if Sys.file_exists dir && Sys.is_directory dir then
      let entries = Sys.readdir dir |> Array.to_list in
      let jsons = List.filter (fun f -> Filename.check_suffix f ".json") entries in
      match jsons with
      | [] -> Error (Printf.sprintf "No .json boards in %s" dir)
      | files ->
          let idx = Random.int (List.length files) in
          let file = List.nth files idx in
          Ok (Filename.concat dir file)
    else Error (Printf.sprintf "Boards directory not found for '%s'" difficulty)
  in

  let choose_board_from_user () =
    let rec loop () =
      print_string "Choose difficulty (easy, medium, hard) or 'quit': ";
      flush stdout;
      try
        match String.lowercase_ascii (String.trim (read_line ())) with
        | "easy" | "medium" | "hard" as d -> (
            match pick_random_board_for d with
            | Ok path -> path
            | Error msg -> prerr_endline ("Error: " ^ msg); loop ())
        | "quit" | "exit" | "q" ->
            print_endline "Goodbye!";
            exit 0
        | other -> prerr_endline ("Invalid difficulty: " ^ other); loop ()
      with End_of_file -> exit 0
    in
    loop ()
  in

  let rec run () =
    let path =
      match args with
      | [ path ] -> path
      | [] -> choose_board_from_user ()
      | _ -> usage ()
    in
    match Sudoku.load_grid path with
    | exception Sudoku.Parse_error msg ->
        prerr_endline ("Error: " ^ msg);
        exit 1
    | original_grid -> (
        match start_game original_grid with
        | Quit -> ()
        | Menu -> run ())
  in
  run ()
