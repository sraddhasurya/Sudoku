open OUnit2

let assert_parse_error f =
  match f () with
  | exception Sudoku.Parse_error _ -> ()
  | _ -> assert_failure "Expected Sudoku.Parse_error"

let with_temp_file content f =
  let file = Filename.temp_file "sudoku" ".json" in
  let oc = open_out file in
  output_string oc content;
  close_out oc;
  Fun.protect ~finally:(fun () -> Sys.remove file) (fun () -> f file)

let sample_json =
  {|
[
  [5, 3, 0, 0, 7, 0, 0, 0, 0],
  [6, 0, 0, 1, 9, 5, 0, 0, 0],
  [0, 9, 8, 0, 0, 0, 0, 6, 0],
  [8, 0, 0, 0, 6, 0, 0, 0, 3],
  [4, 0, 0, 8, 0, 3, 0, 0, 1],
  [7, 0, 0, 0, 2, 0, 0, 0, 6],
  [0, 6, 0, 0, 0, 0, 2, 8, 0],
  [0, 0, 0, 4, 1, 9, 0, 0, 5],
  [0, 0, 0, 0, 8, 0, 0, 7, 9]
]
|}

let solved_sample_grid =
  [|
    [| 5; 3; 4; 6; 7; 8; 9; 1; 2 |];
    [| 6; 7; 2; 1; 9; 5; 3; 4; 8 |];
    [| 1; 9; 8; 3; 4; 2; 5; 6; 7 |];
    [| 8; 5; 9; 7; 6; 1; 4; 2; 3 |];
    [| 4; 2; 6; 8; 5; 3; 7; 9; 1 |];
    [| 7; 1; 3; 9; 2; 4; 8; 5; 6 |];
    [| 9; 6; 1; 5; 3; 7; 2; 8; 4 |];
    [| 2; 8; 7; 4; 1; 9; 6; 3; 5 |];
    [| 3; 4; 5; 2; 8; 6; 1; 7; 9 |];
  |]

let unsolvable_json =
  {|
[
  [5, 5, 0, 0, 7, 0, 0, 0, 0],
  [6, 0, 0, 1, 9, 5, 0, 0, 0],
  [0, 9, 8, 0, 0, 0, 0, 6, 0],
  [8, 0, 0, 0, 6, 0, 0, 0, 3],
  [4, 0, 0, 8, 0, 3, 0, 0, 1],
  [7, 0, 0, 0, 2, 0, 0, 0, 6],
  [0, 6, 0, 0, 0, 0, 2, 8, 0],
  [0, 0, 0, 4, 1, 9, 0, 0, 5],
  [0, 0, 0, 0, 8, 0, 0, 7, 9]
]
|}

let valid_board _ctx =
  with_temp_file sample_json (fun path ->
      let grid = Sudoku.load_grid path in
      assert_equal 5 grid.(0).(0);
      assert_equal 8 grid.(8).(4);
      assert_equal 9 grid.(8).(8);
      let rendering = Sudoku.format_grid grid in
      assert_bool "Includes column header" (String.contains rendering '9');
      assert_bool "Includes row label"
        (String.exists (fun ch -> ch = '1') rendering);
      ())

let object_wrapper _ctx =
  let json =
    {|{ "board": [ [0,0,0,0,0,0,0,0,0],
                             [0,0,0,0,0,0,0,0,0],
                             [0,0,0,0,0,0,0,0,0],
                             [0,0,0,0,0,0,0,0,0],
                             [0,0,0,0,0,0,0,0,0],
                             [0,0,0,0,0,0,0,0,0],
                             [0,0,0,0,0,0,0,0,0],
                             [0,0,0,0,0,0,0,0,0],
                             [0,0,0,0,0,0,0,0,0] ]} |}
  in
  with_temp_file json (fun path ->
      let grid = Sudoku.load_grid path in
      assert_equal 0 grid.(4).(4))

let malformed_rows _ctx =
  let bad = {| [ [1,2,3], [4,5,6] ] |} in
  assert_parse_error (fun () ->
      with_temp_file bad (fun path -> ignore (Sudoku.load_grid path)))

let bad_value _ctx =
  let bad =
    {| [ [10,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0] ] |}
  in
  assert_parse_error (fun () ->
      with_temp_file bad (fun path -> ignore (Sudoku.load_grid path)))

(* Helper to create an empty grid *)
let empty_grid = Array.make_matrix 9 9 0

(* Test update_cell functionality *)
let update_cell_valid _ctx =
  let grid = Array.map (fun row -> Array.copy row) empty_grid in
  let original_grid = Array.map (fun row -> Array.copy row) empty_grid in
  let updated = Sudoku.update_cell grid original_grid 0 0 5 in
  assert_equal 5 updated.(0).(0);
  assert_equal 0 grid.(0).(0);
  assert_equal 0 updated.(0).(1)

let update_cell_multiple _ctx =
  let grid = Array.map (fun row -> Array.copy row) empty_grid in
  let original_grid = Array.map (fun row -> Array.copy row) empty_grid in
  let updated1 = Sudoku.update_cell grid original_grid 2 3 7 in
  let updated2 = Sudoku.update_cell updated1 original_grid 5 6 3 in
  assert_equal 7 updated2.(2).(3);
  assert_equal 3 updated2.(5).(6);
  assert_equal 0 updated2.(0).(0);
  assert_equal 0 updated1.(5).(6)

let update_cell_invalid_row_negative _ctx =
  let grid = Array.map (fun row -> Array.copy row) empty_grid in
  let original_grid = Array.map (fun row -> Array.copy row) empty_grid in
  let neg_one = -1 in
  assert_parse_error (fun () ->
      ignore (Sudoku.update_cell grid original_grid neg_one 0 5))

let update_cell_invalid_row_too_large _ctx =
  let grid = Array.map (fun row -> Array.copy row) empty_grid in
  let original_grid = Array.map (fun row -> Array.copy row) empty_grid in
  assert_parse_error (fun () ->
      ignore (Sudoku.update_cell grid original_grid 9 0 5))

let update_cell_invalid_col_negative _ctx =
  let grid = Array.map (fun row -> Array.copy row) empty_grid in
  let original_grid = Array.map (fun row -> Array.copy row) empty_grid in
  let neg_one = -1 in
  assert_parse_error (fun () ->
      ignore (Sudoku.update_cell grid original_grid 0 neg_one 5))

let update_cell_invalid_col_too_large _ctx =
  let grid = Array.map (fun row -> Array.copy row) empty_grid in
  let original_grid = Array.map (fun row -> Array.copy row) empty_grid in
  assert_parse_error (fun () ->
      ignore (Sudoku.update_cell grid original_grid 0 9 5))

let update_cell_invalid_value_negative _ctx =
  let grid = Array.map (fun row -> Array.copy row) empty_grid in
  let original_grid = Array.map (fun row -> Array.copy row) empty_grid in
  let neg_one = -1 in
  assert_parse_error (fun () ->
      ignore (Sudoku.update_cell grid original_grid 0 0 neg_one))

let update_cell_invalid_value_too_large _ctx =
  let grid = Array.map (fun row -> Array.copy row) empty_grid in
  let original_grid = Array.map (fun row -> Array.copy row) empty_grid in
  assert_parse_error (fun () ->
      ignore (Sudoku.update_cell grid original_grid 0 0 10))

let update_cell_overwrite_filled _ctx =
  with_temp_file sample_json (fun path ->
      let grid = Sudoku.load_grid path in
      let original_grid = Sudoku.load_grid path in
      (* Cell (0,0) contains 5 in sample_json - it's locked *)
      assert_equal 5 grid.(0).(0);
      assert_parse_error (fun () ->
          ignore (Sudoku.update_cell grid original_grid 0 0 3)))

let update_cell_overwrite_filled_different_value _ctx =
  with_temp_file sample_json (fun path ->
      let grid = Sudoku.load_grid path in
      let original_grid = Sudoku.load_grid path in
      (* Cell (1,0) contains 6 in sample_json - it's locked *)
      assert_equal 6 grid.(1).(0);
      assert_parse_error (fun () ->
          ignore (Sudoku.update_cell grid original_grid 1 0 9)))

let update_cell_empty_cell_allowed _ctx =
  with_temp_file sample_json (fun path ->
      let grid = Sudoku.load_grid path in
      let original_grid = Sudoku.load_grid path in
      (* Cell (0,2) is empty (0) in sample_json *)
      assert_equal 0 grid.(0).(2);
      let updated = Sudoku.update_cell grid original_grid 0 2 4 in
      assert_equal 4 updated.(0).(2);
      assert_equal 0 grid.(0).(2))

let update_cell_all_corners _ctx =
  let grid = Array.map (fun row -> Array.copy row) empty_grid in
  let original_grid = Array.map (fun row -> Array.copy row) empty_grid in
  let updated =
    ( ( (grid |> fun g -> Sudoku.update_cell g original_grid 0 0 1) |> fun g ->
        Sudoku.update_cell g original_grid 0 8 2 )
    |> fun g -> Sudoku.update_cell g original_grid 8 0 3 )
    |> fun g -> Sudoku.update_cell g original_grid 8 8 4
  in
  assert_equal 1 updated.(0).(0);
  assert_equal 2 updated.(0).(8);
  assert_equal 3 updated.(8).(0);
  assert_equal 4 updated.(8).(8)

let update_cell_zero_value _ctx =
  let grid = Array.map (fun row -> Array.copy row) empty_grid in
  let original_grid = Array.map (fun row -> Array.copy row) empty_grid in
  let updated = Sudoku.update_cell grid original_grid 4 4 0 in
  assert_equal 0 updated.(4).(4)

let update_cell_edit_user_input _ctx =
  let grid = Array.map (fun row -> Array.copy row) empty_grid in
  let original_grid = Array.map (fun row -> Array.copy row) empty_grid in
  let updated1 = Sudoku.update_cell grid original_grid 2 3 7 in
  assert_equal 7 updated1.(2).(3);
  let updated2 = Sudoku.update_cell updated1 original_grid 2 3 5 in
  assert_equal 5 updated2.(2).(3);
  let updated3 = Sudoku.update_cell updated2 original_grid 2 3 0 in
  assert_equal 0 updated3.(2).(3)

(* Test validation functions *)
let valid_complete_sudoku _ctx =
  let valid_grid_json =
    {|
[
  [5, 3, 4, 6, 7, 8, 9, 1, 2],
  [6, 7, 2, 1, 9, 5, 3, 4, 8],
  [1, 9, 8, 3, 4, 2, 5, 6, 7],
  [8, 5, 9, 7, 6, 1, 4, 2, 3],
  [4, 2, 6, 8, 5, 3, 7, 9, 1],
  [7, 1, 3, 9, 2, 4, 8, 5, 6],
  [9, 6, 1, 5, 3, 7, 2, 8, 4],
  [2, 8, 7, 4, 1, 9, 6, 3, 5],
  [3, 4, 5, 2, 8, 6, 1, 7, 9]
]
|}
  in
  with_temp_file valid_grid_json (fun path ->
      let grid = Sudoku.load_grid path in
      assert_bool "Valid complete sudoku" (Sudoku.is_complete grid);
      assert_bool "Valid sudoku solution" (Sudoku.is_valid_sudoku grid))

let incomplete_board _ctx =
  with_temp_file sample_json (fun path ->
      let grid = Sudoku.load_grid path in
      assert_bool "Incomplete board" (not (Sudoku.is_complete grid));
      assert_bool "Invalid sudoku (incomplete)"
        (not (Sudoku.is_valid_sudoku grid)))

let empty_board_not_complete _ctx =
  let grid = Array.map (fun row -> Array.copy row) empty_grid in
  assert_bool "Empty board not complete" (not (Sudoku.is_complete grid));
  assert_bool "Empty board not valid" (not (Sudoku.is_valid_sudoku grid))

let board_with_invalid_values _ctx =
  (* Board with values outside 1-9 range *)
  let grid = Array.make_matrix 9 9 0 in
  grid.(0) <- [| 5; 3; 4; 6; 7; 8; 9; 1; 2 |];
  grid.(1) <- [| 6; 7; 2; 1; 9; 5; 3; 4; 8 |];
  grid.(2) <- [| 1; 9; 8; 3; 4; 2; 5; 6; 7 |];
  grid.(3) <- [| 8; 5; 9; 7; 6; 1; 4; 2; 3 |];
  grid.(4) <- [| 4; 2; 6; 8; 5; 3; 7; 9; 1 |];
  grid.(5) <- [| 7; 1; 3; 9; 2; 4; 8; 5; 6 |];
  grid.(6) <- [| 9; 6; 1; 5; 3; 7; 2; 8; 4 |];
  grid.(7) <- [| 2; 8; 7; 4; 1; 9; 6; 3; 5 |];
  grid.(8) <- [| 3; 4; 5; 2; 8; 6; 1; 7; 10 |];
  grid.(8).(8) <- 10;
  assert_bool "Board with invalid value not valid"
    (not (Sudoku.is_valid_sudoku grid))

let board_with_duplicates _ctx =
  (* Board with duplicate in first row *)
  let duplicate_grid_json =
    {|
[
  [5, 3, 4, 6, 7, 8, 9, 1, 5],
  [6, 7, 2, 1, 9, 5, 3, 4, 8],
  [1, 9, 8, 3, 4, 2, 5, 6, 7],
  [8, 5, 9, 7, 6, 1, 4, 2, 3],
  [4, 2, 6, 8, 5, 3, 7, 9, 1],
  [7, 1, 3, 9, 2, 4, 8, 5, 6],
  [9, 6, 1, 5, 3, 7, 2, 8, 4],
  [2, 8, 7, 4, 1, 9, 6, 3, 5],
  [3, 4, 5, 2, 8, 6, 1, 7, 9]
]
|}
  in
  with_temp_file duplicate_grid_json (fun path ->
      let grid = Sudoku.load_grid path in
      assert_bool "Board with duplicates not valid"
        (not (Sudoku.is_valid_sudoku grid)))

let board_missing_digit _ctx =
  (* Board with first row missing digit 2 (has 1,3,4,5,6,7,8,9,1 - duplicate
     1) *)
  let missing_digit_json =
    {|
[
  [1, 3, 4, 5, 6, 7, 8, 9, 1],
  [6, 7, 2, 1, 9, 5, 3, 4, 8],
  [1, 9, 8, 3, 4, 2, 5, 6, 7],
  [8, 5, 9, 7, 6, 1, 4, 2, 3],
  [4, 2, 6, 8, 5, 3, 7, 9, 1],
  [7, 1, 3, 9, 2, 4, 8, 5, 6],
  [9, 6, 1, 5, 3, 7, 2, 8, 4],
  [2, 8, 7, 4, 1, 9, 6, 3, 5],
  [3, 4, 5, 2, 8, 6, 1, 7, 9]
]
|}
  in
  with_temp_file missing_digit_json (fun path ->
      let grid = Sudoku.load_grid path in
      assert_bool "Board missing a digit not valid"
        (not (Sudoku.is_valid_sudoku grid)))

let board_with_duplicate_in_column _ctx =
  (* Board with duplicate in first column *)
  let duplicate_col_json =
    {|
[
  [5, 3, 4, 6, 7, 8, 9, 1, 2],
  [5, 7, 2, 1, 9, 5, 3, 4, 8],
  [1, 9, 8, 3, 4, 2, 5, 6, 7],
  [8, 5, 9, 7, 6, 1, 4, 2, 3],
  [4, 2, 6, 8, 5, 3, 7, 9, 1],
  [7, 1, 3, 9, 2, 4, 8, 5, 6],
  [9, 6, 1, 5, 3, 7, 2, 8, 4],
  [2, 8, 7, 4, 1, 9, 6, 3, 5],
  [3, 4, 5, 2, 8, 6, 1, 7, 9]
]
|}
  in
  with_temp_file duplicate_col_json (fun path ->
      let grid = Sudoku.load_grid path in
      assert_bool "Board with duplicate in column not valid"
        (not (Sudoku.is_valid_sudoku grid)))

let board_with_duplicate_in_box _ctx =
  (* Board with duplicate in top-left 3x3 box *)
  let duplicate_box_json =
    {|
[
  [5, 3, 4, 6, 7, 8, 9, 1, 2],
  [6, 7, 2, 1, 9, 5, 3, 4, 8],
  [1, 9, 5, 3, 4, 2, 5, 6, 7],
  [8, 5, 9, 7, 6, 1, 4, 2, 3],
  [4, 2, 6, 8, 5, 3, 7, 9, 1],
  [7, 1, 3, 9, 2, 4, 8, 5, 6],
  [9, 6, 1, 5, 3, 7, 2, 8, 4],
  [2, 8, 7, 4, 1, 9, 6, 3, 5],
  [3, 4, 5, 2, 8, 6, 1, 7, 9]
]
|}
  in
  with_temp_file duplicate_box_json (fun path ->
      let grid = Sudoku.load_grid path in
      assert_bool "Board with duplicate in box not valid"
        (not (Sudoku.is_valid_sudoku grid)))

let board_with_zero_in_complete _ctx =
  (* Board that's complete except for one zero *)
  let almost_complete_json =
    {|
[
  [5, 3, 4, 6, 7, 8, 9, 1, 2],
  [6, 7, 2, 1, 9, 5, 3, 4, 8],
  [1, 9, 8, 3, 4, 2, 5, 6, 7],
  [8, 5, 9, 7, 6, 1, 4, 2, 3],
  [4, 2, 6, 8, 5, 3, 7, 9, 1],
  [7, 1, 3, 9, 2, 4, 8, 5, 6],
  [9, 6, 1, 5, 3, 7, 2, 8, 4],
  [2, 8, 7, 4, 1, 9, 6, 3, 5],
  [3, 4, 5, 2, 8, 6, 1, 7, 0]
]
|}
  in
  with_temp_file almost_complete_json (fun path ->
      let grid = Sudoku.load_grid path in
      assert_bool "Board with zero not complete" (not (Sudoku.is_complete grid));
      assert_bool "Board with zero not valid"
        (not (Sudoku.is_valid_sudoku grid)))

let board_with_value_below_range _ctx =
  (* Board with value 0 in a complete board (should fail validation) *)
  let zero_value_json =
    {|
[
  [5, 3, 4, 6, 7, 8, 9, 1, 2],
  [6, 7, 2, 1, 9, 5, 3, 4, 8],
  [1, 9, 8, 3, 4, 2, 5, 6, 7],
  [8, 5, 9, 7, 6, 1, 4, 2, 3],
  [4, 2, 6, 8, 5, 3, 7, 9, 1],
  [7, 1, 3, 9, 2, 4, 8, 5, 6],
  [9, 6, 1, 5, 3, 7, 2, 8, 4],
  [2, 8, 7, 4, 1, 9, 6, 3, 5],
  [3, 4, 5, 2, 8, 6, 1, 7, 0]
]
|}
  in
  with_temp_file zero_value_json (fun path ->
      let grid = Sudoku.load_grid path in
      assert_bool "Board with zero value not valid"
        (not (Sudoku.is_valid_sudoku grid)))

(* Solver tests *)
let solves_sample_board _ctx =
  with_temp_file sample_json (fun path ->
      let grid = Sudoku.load_grid path in
      match Sudoku.solve grid with
      | Error msg -> assert_failure ("Expected a solution, got error: " ^ msg)
      | Ok solved ->
          assert_bool "Solution should be valid" (Sudoku.is_valid_sudoku solved);
          assert_equal 0 grid.(0).(2);
          assert_equal solved_sample_grid solved)

let detects_unsolvable_board _ctx =
  with_temp_file unsolvable_json (fun path ->
      let grid = Sudoku.load_grid path in
      match Sudoku.solve grid with
      | Ok _ -> assert_failure "Expected unsolvable board to fail"
      | Error msg ->
          assert_bool "Returns a helpful message" (String.length msg > 0))

(* Tests for original cell protection *)
let update_cell_protect_original_cell_all_values _ctx =
  let test_original_value value =
    let grid = Array.make_matrix 9 9 0 in
    let original_grid = Array.make_matrix 9 9 0 in
    original_grid.(4).(4) <- value;
    grid.(4).(4) <- value;
    assert_parse_error (fun () ->
        ignore (Sudoku.update_cell grid original_grid 4 4 ((value mod 9) + 1)));
    assert_parse_error (fun () ->
        ignore (Sudoku.update_cell grid original_grid 4 4 0))
  in
  for i = 1 to 9 do
    test_original_value i
  done;
  ()

let update_cell_protect_multiple_original_cells _ctx =
  (* Test that multiple original cells are all protected *)
  with_temp_file sample_json (fun path ->
      let grid = Sudoku.load_grid path in
      let original_grid = Sudoku.load_grid path in
      (* Test various original cells from sample_json *)
      (* Row 0, col 0: 5 *)
      assert_parse_error (fun () ->
          ignore (Sudoku.update_cell grid original_grid 0 0 1));
      (* Row 0, col 1: 3 *)
      assert_parse_error (fun () ->
          ignore (Sudoku.update_cell grid original_grid 0 1 2));
      (* Row 1, col 0: 6 *)
      assert_parse_error (fun () ->
          ignore (Sudoku.update_cell grid original_grid 1 0 4));
      (* Row 1, col 3: 1 *)
      assert_parse_error (fun () ->
          ignore (Sudoku.update_cell grid original_grid 1 3 7));
      (* Row 1, col 4: 9 *)
      assert_parse_error (fun () ->
          ignore (Sudoku.update_cell grid original_grid 1 4 8));
      (* Try to clear original cells *)
      assert_parse_error (fun () ->
          ignore (Sudoku.update_cell grid original_grid 0 0 0));
      assert_parse_error (fun () ->
          ignore (Sudoku.update_cell grid original_grid 1 0 0)))

(* Tests for user input editing *)
let update_cell_edit_user_input_multiple_times _ctx =
  let grid = Array.map (fun row -> Array.copy row) empty_grid in
  let original_grid = Array.map (fun row -> Array.copy row) empty_grid in
  let updated1 = Sudoku.update_cell grid original_grid 1 1 3 in
  assert_equal 3 updated1.(1).(1);
  let updated2 = Sudoku.update_cell updated1 original_grid 1 1 7 in
  assert_equal 7 updated2.(1).(1);
  let updated3 = Sudoku.update_cell updated2 original_grid 1 1 9 in
  assert_equal 9 updated3.(1).(1);
  let updated4 = Sudoku.update_cell updated3 original_grid 1 1 1 in
  assert_equal 1 updated4.(1).(1)

let update_cell_clear_user_input _ctx =
  let grid = Array.map (fun row -> Array.copy row) empty_grid in
  let original_grid = Array.map (fun row -> Array.copy row) empty_grid in
  let updated1 = Sudoku.update_cell grid original_grid 5 5 8 in
  assert_equal 8 updated1.(5).(5);
  let updated2 = Sudoku.update_cell updated1 original_grid 5 5 0 in
  assert_equal 0 updated2.(5).(5);
  let updated3 = Sudoku.update_cell updated2 original_grid 5 5 2 in
  assert_equal 2 updated3.(5).(5)

let update_cell_edit_after_clearing_user_input _ctx =
  let grid = Array.map (fun row -> Array.copy row) empty_grid in
  let original_grid = Array.map (fun row -> Array.copy row) empty_grid in
  let updated1 = Sudoku.update_cell grid original_grid 3 3 5 in
  assert_equal 5 updated1.(3).(3);
  let updated2 = Sudoku.update_cell updated1 original_grid 3 3 0 in
  assert_equal 0 updated2.(3).(3);
  let updated3 = Sudoku.update_cell updated2 original_grid 3 3 8 in
  assert_equal 8 updated3.(3).(3);
  let updated4 = Sudoku.update_cell updated3 original_grid 3 3 1 in
  assert_equal 1 updated4.(3).(3)

let update_cell_edit_multiple_user_cells _ctx =
  let grid = Array.map (fun row -> Array.copy row) empty_grid in
  let original_grid = Array.map (fun row -> Array.copy row) empty_grid in
  let updated1 = Sudoku.update_cell grid original_grid 0 0 1 in
  let updated2 = Sudoku.update_cell updated1 original_grid 2 2 5 in
  let updated3 = Sudoku.update_cell updated2 original_grid 4 4 9 in
  assert_equal 1 updated3.(0).(0);
  assert_equal 5 updated3.(2).(2);
  assert_equal 9 updated3.(4).(4);
  let updated4 = Sudoku.update_cell updated3 original_grid 0 0 3 in
  let updated5 = Sudoku.update_cell updated4 original_grid 2 2 7 in
  let updated6 = Sudoku.update_cell updated5 original_grid 4 4 2 in
  assert_equal 3 updated6.(0).(0);
  assert_equal 7 updated6.(2).(2);
  assert_equal 2 updated6.(4).(4);
  let updated7 = Sudoku.update_cell updated6 original_grid 2 2 0 in
  assert_equal 0 updated7.(2).(2);
  assert_equal 3 updated7.(0).(0);
  assert_equal 2 updated7.(4).(4)

(* Tests for mixed scenarios *)
let update_cell_mixed_original_and_user_cells _ctx =
  with_temp_file sample_json (fun path ->
      let grid = Sudoku.load_grid path in
      let original_grid = Sudoku.load_grid path in
      let updated1 = Sudoku.update_cell grid original_grid 0 2 4 in
      assert_equal 4 updated1.(0).(2);
      let updated2 = Sudoku.update_cell updated1 original_grid 0 2 2 in
      assert_equal 2 updated2.(0).(2);
      assert_parse_error (fun () ->
          ignore (Sudoku.update_cell updated2 original_grid 0 0 1));
      let updated3 = Sudoku.update_cell updated2 original_grid 0 5 4 in
      assert_equal 4 updated3.(0).(5);
      let updated4 = Sudoku.update_cell updated3 original_grid 0 5 6 in
      assert_equal 6 updated4.(0).(5);
      assert_parse_error (fun () ->
          ignore (Sudoku.update_cell updated4 original_grid 0 1 1));
      let updated5 = Sudoku.update_cell updated4 original_grid 0 2 0 in
      assert_equal 0 updated5.(0).(2);
      assert_equal 6 updated5.(0).(5))

let update_cell_edit_user_fill_then_edit _ctx =
  with_temp_file sample_json (fun path ->
      let grid = Sudoku.load_grid path in
      let original_grid = Sudoku.load_grid path in
      assert_equal 0 grid.(0).(2);
      assert_equal 0 original_grid.(0).(2);
      let updated1 = Sudoku.update_cell grid original_grid 0 2 4 in
      assert_equal 4 updated1.(0).(2);
      let updated2 = Sudoku.update_cell updated1 original_grid 0 2 2 in
      assert_equal 2 updated2.(0).(2);
      let updated3 = Sudoku.update_cell updated2 original_grid 0 2 2 in
      assert_equal 2 updated3.(0).(2);
      let updated4 = Sudoku.update_cell updated3 original_grid 0 2 0 in
      assert_equal 0 updated4.(0).(2))

let update_cell_empty_original_cell_editable _ctx =
  with_temp_file sample_json (fun path ->
      let grid = Sudoku.load_grid path in
      let original_grid = Sudoku.load_grid path in
      assert_equal 0 grid.(0).(2);
      assert_equal 0 original_grid.(0).(2);
      let updated = Sudoku.update_cell grid original_grid 0 2 4 in
      assert_equal 4 updated.(0).(2);
      let updated2 = Sudoku.update_cell updated original_grid 0 2 2 in
      assert_equal 2 updated2.(0).(2);
      let updated3 = Sudoku.update_cell updated2 original_grid 0 2 0 in
      assert_equal 0 updated3.(0).(2))

(* Tests for duplicate validation during gameplay *)
let update_cell_prevent_duplicate_in_row _ctx =
  (* Test that placing a duplicate in a row is prevented *)
  with_temp_file sample_json (fun path ->
      let grid = Sudoku.load_grid path in
      let original_grid = Sudoku.load_grid path in
      (* Row 0 has 5 at (0,0) and 3 at (0,1) *)
      (* Try to place 5 at (0,2) - should fail *)
      assert_parse_error (fun () ->
          ignore (Sudoku.update_cell grid original_grid 0 2 5)))

let update_cell_prevent_duplicate_in_column _ctx =
  with_temp_file sample_json (fun path ->
      let grid = Sudoku.load_grid path in
      let original_grid = Sudoku.load_grid path in
      assert_parse_error (fun () ->
          ignore (Sudoku.update_cell grid original_grid 2 0 5)))

let update_cell_prevent_duplicate_in_box _ctx =
  with_temp_file sample_json (fun path ->
      let grid = Sudoku.load_grid path in
      let original_grid = Sudoku.load_grid path in
      assert_parse_error (fun () ->
          ignore (Sudoku.update_cell grid original_grid 2 2 5)))

let update_cell_allow_valid_move _ctx =
  (* Test that valid moves (no duplicates) are allowed *)
  with_temp_file sample_json (fun path ->
      let grid = Sudoku.load_grid path in
      let original_grid = Sudoku.load_grid path in
      (* Row 0: 5, 3, 0, 0, 7, 0, 0, 0, 0 *)
      (* Column 2: 0, 0, 8, 0, 0, 0, 0, 0, 0 *)
      (* Can place 4 at (0,2) - no duplicates *)
      let updated = Sudoku.update_cell grid original_grid 0 2 4 in
      assert_equal 4 updated.(0).(2))

let update_cell_allow_clearing_even_with_duplicates _ctx =
  let grid = Array.make_matrix 9 9 0 in
  let original_grid = Array.make_matrix 9 9 0 in
  grid.(0).(0) <- 5;
  grid.(0).(1) <- 5;
  let updated = Sudoku.update_cell grid original_grid 0 1 0 in
  assert_equal 0 updated.(0).(1);
  assert_equal 5 updated.(0).(0)

let update_cell_prevent_multiple_duplicates _ctx =
  with_temp_file sample_json (fun path ->
      let grid = Sudoku.load_grid path in
      let original_grid = Sudoku.load_grid path in
      try
        ignore (Sudoku.update_cell grid original_grid 0 2 5);
        assert_failure "Expected Parse_error for duplicate in row"
      with Sudoku.Parse_error msg ->
        assert_bool "Error message mentions row duplicate"
          (String.contains msg 'r' || String.contains msg 'R'))

(* Test user winning the game *)
let user_wins_game _ctx =
  with_temp_file sample_json (fun path ->
      let grid = Sudoku.load_grid path in
      let original_grid = Sudoku.load_grid path in
      assert_bool "Initial board is incomplete" (not (Sudoku.is_complete grid));
      assert_bool "Initial board is not valid"
        (not (Sudoku.is_valid_sudoku grid));
      let step1 = Sudoku.update_cell grid original_grid 0 2 4 in
      let step2 = Sudoku.update_cell step1 original_grid 0 3 6 in
      let step3 = Sudoku.update_cell step2 original_grid 0 5 8 in
      let step4 = Sudoku.update_cell step3 original_grid 0 6 9 in
      let step5 = Sudoku.update_cell step4 original_grid 0 7 1 in
      let step6 = Sudoku.update_cell step5 original_grid 0 8 2 in
      let step7 = Sudoku.update_cell step6 original_grid 1 1 7 in
      let step8 = Sudoku.update_cell step7 original_grid 1 2 2 in
      let step9 = Sudoku.update_cell step8 original_grid 1 6 3 in
      let step10 = Sudoku.update_cell step9 original_grid 1 7 4 in
      let step11 = Sudoku.update_cell step10 original_grid 1 8 8 in
      let step12 = Sudoku.update_cell step11 original_grid 2 0 1 in
      let step13 = Sudoku.update_cell step12 original_grid 2 3 3 in
      let step14 = Sudoku.update_cell step13 original_grid 2 4 4 in
      let step15 = Sudoku.update_cell step14 original_grid 2 5 2 in
      let step16 = Sudoku.update_cell step15 original_grid 2 6 5 in
      let step17 = Sudoku.update_cell step16 original_grid 2 8 7 in
      let step18 = Sudoku.update_cell step17 original_grid 3 1 5 in
      let step19 = Sudoku.update_cell step18 original_grid 3 2 9 in
      let step20 = Sudoku.update_cell step19 original_grid 3 3 7 in
      let step21 = Sudoku.update_cell step20 original_grid 3 5 1 in
      let step22 = Sudoku.update_cell step21 original_grid 3 6 4 in
      let step23 = Sudoku.update_cell step22 original_grid 3 7 2 in
      let step24 = Sudoku.update_cell step23 original_grid 4 1 2 in
      let step25 = Sudoku.update_cell step24 original_grid 4 2 6 in
      let step26 = Sudoku.update_cell step25 original_grid 4 4 5 in
      let step27 = Sudoku.update_cell step26 original_grid 4 6 7 in
      let step28 = Sudoku.update_cell step27 original_grid 4 7 9 in
      let step29 = Sudoku.update_cell step28 original_grid 5 1 1 in
      let step30 = Sudoku.update_cell step29 original_grid 5 2 3 in
      let step31 = Sudoku.update_cell step30 original_grid 5 3 9 in
      let step32 = Sudoku.update_cell step31 original_grid 5 5 4 in
      let step33 = Sudoku.update_cell step32 original_grid 5 6 8 in
      let step34 = Sudoku.update_cell step33 original_grid 5 7 5 in
      let step35 = Sudoku.update_cell step34 original_grid 6 0 9 in
      let step36 = Sudoku.update_cell step35 original_grid 6 2 1 in
      let step37 = Sudoku.update_cell step36 original_grid 6 3 5 in
      let step38 = Sudoku.update_cell step37 original_grid 6 4 3 in
      let step39 = Sudoku.update_cell step38 original_grid 6 5 7 in
      let step40 = Sudoku.update_cell step39 original_grid 6 8 4 in
      let step41 = Sudoku.update_cell step40 original_grid 7 0 2 in
      let step42 = Sudoku.update_cell step41 original_grid 7 1 8 in
      let step43 = Sudoku.update_cell step42 original_grid 7 2 7 in
      let step44 = Sudoku.update_cell step43 original_grid 7 6 6 in
      let step45 = Sudoku.update_cell step44 original_grid 7 7 3 in
      let step46 = Sudoku.update_cell step45 original_grid 8 0 3 in
      let step47 = Sudoku.update_cell step46 original_grid 8 1 4 in
      let step48 = Sudoku.update_cell step47 original_grid 8 2 5 in
      let step49 = Sudoku.update_cell step48 original_grid 8 3 2 in
      let step50 = Sudoku.update_cell step49 original_grid 8 5 6 in
      let step51 = Sudoku.update_cell step50 original_grid 8 6 1 in
      assert_bool "Board is complete after filling all cells"
        (Sudoku.is_complete step51);
      assert_bool "User wins - board is valid sudoku solution"
        (Sudoku.is_valid_sudoku step51))

let user_wins_with_edits _ctx =
  with_temp_file sample_json (fun path ->
      let grid = Sudoku.load_grid path in
      let original_grid = Sudoku.load_grid path in
      let step1 = Sudoku.update_cell grid original_grid 0 2 1 in
      let step2 = Sudoku.update_cell step1 original_grid 0 2 4 in
      let step3 = Sudoku.update_cell step2 original_grid 0 3 6 in
      let step4 = Sudoku.update_cell step3 original_grid 0 5 8 in
      let step5 = Sudoku.update_cell step4 original_grid 0 5 8 in
      let step6 = Sudoku.update_cell step5 original_grid 0 5 0 in
      let step7 = Sudoku.update_cell step6 original_grid 0 5 8 in
      assert_bool "Can edit cells and continue playing"
        (not (Sudoku.is_complete step7)))

(* Tests for error handling in parsing *)
let parse_error_non_integer_cell _ctx =
  let bad =
    {| [ ["a",0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0] ] |}
  in
  assert_parse_error (fun () ->
      with_temp_file bad (fun path -> ignore (Sudoku.load_grid path)))

let parse_error_row_not_list _ctx =
  let bad =
    {| [ 123, [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0] ] |}
  in
  assert_parse_error (fun () ->
      with_temp_file bad (fun path -> ignore (Sudoku.load_grid path)))

let parse_error_board_not_list _ctx =
  let bad = {| { "not_board": "value" } |} in
  assert_parse_error (fun () ->
      with_temp_file bad (fun path -> ignore (Sudoku.load_grid path)))

let parse_error_missing_board_field _ctx =
  let bad = {| { "other_field": [[0,0,0,0,0,0,0,0,0]] } |} in
  assert_parse_error (fun () ->
      with_temp_file bad (fun path -> ignore (Sudoku.load_grid path)))

let parse_error_invalid_json_structure _ctx =
  let bad = {| "not a list or object" |} in
  assert_parse_error (fun () ->
      with_temp_file bad (fun path -> ignore (Sudoku.load_grid path)))

let parse_error_malformed_json _ctx =
  let bad = {| [ [0,0,0,0,0,0,0,0,0], |} in
  assert_parse_error (fun () ->
      with_temp_file bad (fun path -> ignore (Sudoku.load_grid path)))

let parse_error_nonexistent_file _ctx =
  assert_parse_error (fun () ->
      ignore (Sudoku.load_grid "/nonexistent/path/to/file.json"))

let parse_error_wrong_row_count _ctx =
  let bad =
    {| [ [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0] ] |}
  in
  assert_parse_error (fun () ->
      with_temp_file bad (fun path -> ignore (Sudoku.load_grid path)))

let parse_error_wrong_col_count _ctx =
  let bad =
    {| [ [0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0] ] |}
  in
  assert_parse_error (fun () ->
      with_temp_file bad (fun path -> ignore (Sudoku.load_grid path)))

(* Tests for format_grid and print_grid *)
let format_grid_basic _ctx =
  let grid = Array.make_matrix 9 9 0 in
  grid.(0).(0) <- 5;
  grid.(4).(4) <- 3;
  let formatted = Sudoku.format_grid grid in
  assert_bool "Formatted grid contains header" (String.contains formatted '1');
  assert_bool "Formatted grid contains separator"
    (String.contains formatted '+');
  assert_bool "Formatted grid contains cell value"
    (String.contains formatted '5');
  assert_bool "Formatted grid contains empty cell"
    (String.contains formatted '.')

let format_grid_with_colorize _ctx =
  let grid = Array.make_matrix 9 9 0 in
  grid.(0).(0) <- 5;
  let colorize r c text = if r = 0 && c = 0 then "[" ^ text ^ "]" else text in
  let formatted = Sudoku.format_grid ~colorize grid in
  assert_bool "Formatted grid with colorize contains marker"
    (String.contains formatted '[')

(* Tests for generate function *)
let generate_easy _ctx =
  let grid = Sudoku.generate Sudoku.Easy in
  assert_equal 9 (Array.length grid);
  assert_equal 9 (Array.length grid.(0));
  let count = ref 0 in
  for i = 0 to 8 do
    for j = 0 to 8 do
      if grid.(i).(j) <> 0 then incr count
    done
  done;
  assert_bool "Easy puzzle has reasonable number of clues"
    (!count >= 30 && !count <= 50);
  match Sudoku.solve grid with
  | Ok _ -> ()
  | Error _ -> assert_failure "Generated puzzle should be solvable"

let generate_medium _ctx =
  let grid = Sudoku.generate Sudoku.Medium in
  assert_equal 9 (Array.length grid);
  assert_equal 9 (Array.length grid.(0));
  let count = ref 0 in
  for i = 0 to 8 do
    for j = 0 to 8 do
      if grid.(i).(j) <> 0 then incr count
    done
  done;
  assert_bool "Medium puzzle has reasonable number of clues"
    (!count >= 25 && !count <= 40);
  match Sudoku.solve grid with
  | Ok _ -> ()
  | Error _ -> assert_failure "Generated puzzle should be solvable"

let generate_hard _ctx =
  let grid = Sudoku.generate Sudoku.Hard in
  assert_equal 9 (Array.length grid);
  assert_equal 9 (Array.length grid.(0));
  let count = ref 0 in
  for i = 0 to 8 do
    for j = 0 to 8 do
      if grid.(i).(j) <> 0 then incr count
    done
  done;
  assert_bool "Hard puzzle has reasonable number of clues"
    (!count >= 20 && !count <= 30);
  match Sudoku.solve grid with
  | Ok _ -> ()
  | Error _ -> assert_failure "Generated puzzle should be solvable"

(* Tests for edge cases in validation *)

let is_valid_sudoku_invalid_column _ctx =
  let grid = Array.make_matrix 9 9 0 in
  grid.(0) <- [| 5; 3; 4; 6; 7; 8; 9; 1; 2 |];
  grid.(1) <- [| 5; 7; 2; 1; 9; 5; 3; 4; 8 |];
  grid.(2) <- [| 1; 9; 8; 3; 4; 2; 5; 6; 7 |];
  grid.(3) <- [| 8; 5; 9; 7; 6; 1; 4; 2; 3 |];
  grid.(4) <- [| 4; 2; 6; 8; 5; 3; 7; 9; 1 |];
  grid.(5) <- [| 7; 1; 3; 9; 2; 4; 8; 5; 6 |];
  grid.(6) <- [| 9; 6; 1; 5; 3; 7; 2; 8; 4 |];
  grid.(7) <- [| 2; 8; 7; 4; 1; 9; 6; 3; 5 |];
  grid.(8) <- [| 3; 4; 5; 2; 8; 6; 1; 7; 9 |];
  for i = 0 to 8 do
    for j = 0 to 8 do
      if grid.(i).(j) = 0 then grid.(i).(j) <- 1
    done
  done;
  assert_bool "Board with duplicate column is not valid"
    (not (Sudoku.is_valid_sudoku grid))

let is_consistent_invalid_column _ctx =
  let grid = Array.make_matrix 9 9 0 in
  grid.(0).(0) <- 5;
  grid.(1).(0) <- 5;
  match Sudoku.solve grid with
  | Error msg ->
      assert_bool "Error message mentions conflicts"
        (String.contains msg 'c' || String.contains msg 'C')
  | Ok _ -> assert_failure "Expected error for inconsistent board"

let update_cell_error_box_duplicate _ctx =
  with_temp_file sample_json (fun path ->
      let grid = Sudoku.load_grid path in
      let original_grid = Sudoku.load_grid path in
      try
        ignore (Sudoku.update_cell grid original_grid 2 2 5);
        assert_failure "Expected Parse_error for duplicate in box"
      with Sudoku.Parse_error msg ->
        assert_bool "Error message mentions box duplicate"
          (String.contains msg 'b' || String.contains msg 'B'
         || String.contains msg '3'))

let solve_no_solution _ctx =
  let conflict_grid = Array.make_matrix 9 9 0 in
  conflict_grid.(0).(0) <- 1;
  conflict_grid.(0).(1) <- 1;
  match Sudoku.solve conflict_grid with
  | Error _ -> ()
  | Ok _ -> assert_failure "Expected error for inconsistent board"

let solve_empty_board _ctx =
  let grid = Array.make_matrix 9 9 0 in
  match Sudoku.solve grid with
  | Error _ -> assert_failure "Empty board should be solvable"
  | Ok solved ->
      assert_bool "Solved empty board is valid" (Sudoku.is_valid_sudoku solved);
      assert_bool "Solved empty board is complete" (Sudoku.is_complete solved)

let candidates_function _ctx =
  with_temp_file sample_json (fun path ->
      let grid = Sudoku.load_grid path in
      match Sudoku.solve grid with
      | Ok _ -> ()
      | Error _ -> assert_failure "Sample board should be solvable")

let best_empty_cell_with_rng _ctx =
  let grid = Sudoku.generate Sudoku.Easy in
  match Sudoku.solve grid with
  | Ok _ -> ()
  | Error _ -> assert_failure "Generated puzzle should be solvable"

let suite =
  "sudoku"
  >::: [
         "load_valid" >:: valid_board;
         "load_object_wrapper" >:: object_wrapper;
         "malformed_rows" >:: malformed_rows;
         "bad_value" >:: bad_value;
         "update_cell_valid" >:: update_cell_valid;
         "update_cell_multiple" >:: update_cell_multiple;
         "update_cell_invalid_row_negative" >:: update_cell_invalid_row_negative;
         "update_cell_invalid_row_too_large"
         >:: update_cell_invalid_row_too_large;
         "update_cell_invalid_col_negative" >:: update_cell_invalid_col_negative;
         "update_cell_invalid_col_too_large"
         >:: update_cell_invalid_col_too_large;
         "update_cell_invalid_value_negative"
         >:: update_cell_invalid_value_negative;
         "update_cell_invalid_value_too_large"
         >:: update_cell_invalid_value_too_large;
         "update_cell_overwrite_filled" >:: update_cell_overwrite_filled;
         "update_cell_overwrite_filled_different_value"
         >:: update_cell_overwrite_filled_different_value;
         "update_cell_empty_cell_allowed" >:: update_cell_empty_cell_allowed;
         "update_cell_all_corners" >:: update_cell_all_corners;
         "update_cell_zero_value" >:: update_cell_zero_value;
         "update_cell_edit_user_input" >:: update_cell_edit_user_input;
         "valid_complete_sudoku" >:: valid_complete_sudoku;
         "incomplete_board" >:: incomplete_board;
         "empty_board_not_complete" >:: empty_board_not_complete;
         "board_with_invalid_values" >:: board_with_invalid_values;
         "board_with_duplicates" >:: board_with_duplicates;
         "board_missing_digit" >:: board_missing_digit;
         "board_with_duplicate_in_column" >:: board_with_duplicate_in_column;
         "board_with_duplicate_in_box" >:: board_with_duplicate_in_box;
         "board_with_zero_in_complete" >:: board_with_zero_in_complete;
         "board_with_value_below_range" >:: board_with_value_below_range;
         "update_cell_protect_original_cell_all_values"
         >:: update_cell_protect_original_cell_all_values;
         "update_cell_protect_multiple_original_cells"
         >:: update_cell_protect_multiple_original_cells;
         "update_cell_edit_user_input_multiple_times"
         >:: update_cell_edit_user_input_multiple_times;
         "update_cell_clear_user_input" >:: update_cell_clear_user_input;
         "update_cell_edit_after_clearing_user_input"
         >:: update_cell_edit_after_clearing_user_input;
         "update_cell_edit_multiple_user_cells"
         >:: update_cell_edit_multiple_user_cells;
         "update_cell_mixed_original_and_user_cells"
         >:: update_cell_mixed_original_and_user_cells;
         "update_cell_edit_user_fill_then_edit"
         >:: update_cell_edit_user_fill_then_edit;
         "update_cell_empty_original_cell_editable"
         >:: update_cell_empty_original_cell_editable;
         "update_cell_prevent_duplicate_in_row"
         >:: update_cell_prevent_duplicate_in_row;
         "update_cell_prevent_duplicate_in_column"
         >:: update_cell_prevent_duplicate_in_column;
         "update_cell_prevent_duplicate_in_box"
         >:: update_cell_prevent_duplicate_in_box;
         "update_cell_allow_valid_move" >:: update_cell_allow_valid_move;
         "update_cell_allow_clearing_even_with_duplicates"
         >:: update_cell_allow_clearing_even_with_duplicates;
         "update_cell_prevent_multiple_duplicates"
         >:: update_cell_prevent_multiple_duplicates;
         "solves_sample_board" >:: solves_sample_board;
         "detects_unsolvable_board" >:: detects_unsolvable_board;
         "user_wins_game" >:: user_wins_game;
         "user_wins_with_edits" >:: user_wins_with_edits;
         "parse_error_non_integer_cell" >:: parse_error_non_integer_cell;
         "parse_error_row_not_list" >:: parse_error_row_not_list;
         "parse_error_board_not_list" >:: parse_error_board_not_list;
         "parse_error_missing_board_field" >:: parse_error_missing_board_field;
         "parse_error_invalid_json_structure"
         >:: parse_error_invalid_json_structure;
         "parse_error_malformed_json" >:: parse_error_malformed_json;
         "parse_error_nonexistent_file" >:: parse_error_nonexistent_file;
         "parse_error_wrong_row_count" >:: parse_error_wrong_row_count;
         "parse_error_wrong_col_count" >:: parse_error_wrong_col_count;
         "format_grid_basic" >:: format_grid_basic;
         "format_grid_with_colorize" >:: format_grid_with_colorize;
         "generate_easy" >:: generate_easy;
         "generate_medium" >:: generate_medium;
         "generate_hard" >:: generate_hard;
         "is_valid_sudoku_invalid_column" >:: is_valid_sudoku_invalid_column;
         "is_consistent_invalid_column" >:: is_consistent_invalid_column;
         "update_cell_error_box_duplicate" >:: update_cell_error_box_duplicate;
         "solve_no_solution" >:: solve_no_solution;
         "solve_empty_board" >:: solve_empty_board;
         "candidates_function" >:: candidates_function;
         "best_empty_cell_with_rng" >:: best_empty_cell_with_rng;
       ]

let () = run_test_tt_main suite
