Name/NetId: 
1. Rohan Sahu (rs2649)
2. Pragya Babbar (pb597)
3. Sraddha Suryadevara (sss335)
4. Amr Elhady (ae429)

GenAI Usage:
We used ChatGPT primarily as a debugging and learning tool. It helped us interpret OCaml/dune errors and Yojson parse exceptions while building Sudoku.load_grid and format_grid, and it guided us toward the right checks when building validation helpers (contains_all_digits_1_to_9, is_valid_sudoku). It also helped us come up with useful test cases to add to verify that our logic is working thus far. Beyond that, it suggested backtracking-and-shuffle approaches for the puzzle generator, clue-removal counts by difficulty, and CLI/input-validation flows (autocorrect prompts, mistake tracking, and non-blocking highlighting). We used it as a quick OCaml idiom refresher during refactors, and every suggestion was reviewed and tested before inclusion.
