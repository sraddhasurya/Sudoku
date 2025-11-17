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
      assert_bool "Includes row label" (String.exists (fun ch -> ch = '1') rendering);
      ())

let object_wrapper _ctx =
  let json = {|{ "board": [ [0,0,0,0,0,0,0,0,0],
                             [0,0,0,0,0,0,0,0,0],
                             [0,0,0,0,0,0,0,0,0],
                             [0,0,0,0,0,0,0,0,0],
                             [0,0,0,0,0,0,0,0,0],
                             [0,0,0,0,0,0,0,0,0],
                             [0,0,0,0,0,0,0,0,0],
                             [0,0,0,0,0,0,0,0,0],
                             [0,0,0,0,0,0,0,0,0] ]} |} in
  with_temp_file json (fun path ->
      let grid = Sudoku.load_grid path in
      assert_equal 0 grid.(4).(4))

let malformed_rows _ctx =
  let bad = {| [ [1,2,3], [4,5,6] ] |} in
  assert_parse_error (fun () ->
      with_temp_file bad (fun path -> ignore (Sudoku.load_grid path)))

let bad_value _ctx =
  let bad = {| [ [10,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0] ] |} in
  assert_parse_error (fun () ->
      with_temp_file bad (fun path -> ignore (Sudoku.load_grid path)))

let suite =
  "sudoku"
  >::: [
         "load_valid" >:: valid_board;
         "load_object_wrapper" >:: object_wrapper;
         "malformed_rows" >:: malformed_rows;
         "bad_value" >:: bad_value;
       ]

let () = run_test_tt_main suite
