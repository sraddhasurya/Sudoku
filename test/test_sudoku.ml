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
  let updated = Sudoku.update_cell grid 0 0 5 in
  assert_equal 5 updated.(0).(0);
  assert_equal 0 grid.(0).(0);
  (* Original grid unchanged *)
  assert_equal 0 updated.(0).(1)
(* Other cells unchanged *)

let update_cell_multiple _ctx =
  let grid = Array.map (fun row -> Array.copy row) empty_grid in
  let updated1 = Sudoku.update_cell grid 2 3 7 in
  let updated2 = Sudoku.update_cell updated1 5 6 3 in
  assert_equal 7 updated2.(2).(3);
  assert_equal 3 updated2.(5).(6);
  assert_equal 0 updated2.(0).(0);
  assert_equal 0 updated1.(5).(6)
(* First update doesn't affect second cell *)

let update_cell_invalid_row_negative _ctx =
  let grid = Array.map (fun row -> Array.copy row) empty_grid in
  let neg_one = -1 in
  assert_parse_error (fun () -> ignore (Sudoku.update_cell grid neg_one 0 5))

let update_cell_invalid_row_too_large _ctx =
  let grid = Array.map (fun row -> Array.copy row) empty_grid in
  assert_parse_error (fun () -> ignore (Sudoku.update_cell grid 9 0 5))

let update_cell_invalid_col_negative _ctx =
  let grid = Array.map (fun row -> Array.copy row) empty_grid in
  let neg_one = -1 in
  assert_parse_error (fun () -> ignore (Sudoku.update_cell grid 0 neg_one 5))

let update_cell_invalid_col_too_large _ctx =
  let grid = Array.map (fun row -> Array.copy row) empty_grid in
  assert_parse_error (fun () -> ignore (Sudoku.update_cell grid 0 9 5))

let update_cell_invalid_value_negative _ctx =
  let grid = Array.map (fun row -> Array.copy row) empty_grid in
  let neg_one = -1 in
  assert_parse_error (fun () -> ignore (Sudoku.update_cell grid 0 0 neg_one))

let update_cell_invalid_value_too_large _ctx =
  let grid = Array.map (fun row -> Array.copy row) empty_grid in
  assert_parse_error (fun () -> ignore (Sudoku.update_cell grid 0 0 10))

let update_cell_overwrite_filled _ctx =
  with_temp_file sample_json (fun path ->
      let grid = Sudoku.load_grid path in
      (* Cell (0,0) contains 5 in sample_json *)
      assert_equal 5 grid.(0).(0);
      assert_parse_error (fun () -> ignore (Sudoku.update_cell grid 0 0 3)))

let update_cell_overwrite_filled_different_value _ctx =
  with_temp_file sample_json (fun path ->
      let grid = Sudoku.load_grid path in
      (* Cell (1,0) contains 6 in sample_json *)
      assert_equal 6 grid.(1).(0);
      assert_parse_error (fun () -> ignore (Sudoku.update_cell grid 1 0 9)))

let update_cell_empty_cell_allowed _ctx =
  with_temp_file sample_json (fun path ->
      let grid = Sudoku.load_grid path in
      (* Cell (0,2) is empty (0) in sample_json *)
      assert_equal 0 grid.(0).(2);
      let updated = Sudoku.update_cell grid 0 2 4 in
      assert_equal 4 updated.(0).(2);
      assert_equal 0 grid.(0).(2))
(* Original unchanged *)

let update_cell_all_corners _ctx =
  let grid = Array.map (fun row -> Array.copy row) empty_grid in
  let updated =
    ( ( (grid |> fun g -> Sudoku.update_cell g 0 0 1) |> fun g ->
        Sudoku.update_cell g 0 8 2 )
    |> fun g -> Sudoku.update_cell g 8 0 3 )
    |> fun g -> Sudoku.update_cell g 8 8 4
  in
  assert_equal 1 updated.(0).(0);
  assert_equal 2 updated.(0).(8);
  assert_equal 3 updated.(8).(0);
  assert_equal 4 updated.(8).(8)

let update_cell_zero_value _ctx =
  let grid = Array.map (fun row -> Array.copy row) empty_grid in
  (* Setting a cell to 0 should be allowed on an empty grid *)
  let updated = Sudoku.update_cell grid 4 4 0 in
  assert_equal 0 updated.(4).(4)

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
       ]

let () = run_test_tt_main suite
