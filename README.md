# Sudoku

This is a fully featured, terminal-based Sudoku game designed to provide an interactive and polished puzzle-solving experience. The system combines rigorous Sudoku validation, intelligent assistance features, and a user-friendly interface to create a complete, playable game rather than a simple solver or checker.

---

## Overview

The project supports interactive gameplay across multiple difficulty levels, real-time validation, and advanced assistance tools such as hints and autocorrect mode. It emphasizes clean separation of concerns, robust error handling, and a smooth user experience within a terminal environment.

---

## Features

### Multiple Boards and Difficulty Levels
- 15 pre-generated Sudoku boards
- Three difficulty levels: easy, medium, and hard
- Random board selection within each difficulty
- Original puzzle clues are immutable during gameplay

---

### Interactive Gameplay
- Fully interactive terminal-based interface
- Users can place, edit, and erase numbers
- Clear-board functionality to reset all user-entered values
- Improved and forgiving input format
- Menu-driven navigation for game actions and difficulty selection

---

### Sudoku Validation and Logic Checking
- Enforces all standard Sudoku constraints:
  - Row uniqueness
  - Column uniqueness
  - 3×3 subgrid uniqueness
- Distinguishes between original puzzle values and user input
- Detects invalid moves and incomplete or incorrect boards
- Clear and informative error messages for invalid actions

---

### Backtracking Solver
- Implements a backtracking-based Sudoku solver
- Used to verify puzzle solvability and completed solutions
- Can provide a full solution when requested
- Serves as the foundation for hint and autocorrect functionality

---

### Hint System
- Fills a random empty cell with a correct value
- Uses the solver to guarantee correctness
- Allows players to progress without revealing the full solution

---

### Autocorrect Mode and Mistake Tracking
- Optional autocorrect mode compares user input against the solved board
- Incorrect entries are visually highlighted in real time
- Includes a mistake counter that ends the game after three incorrect moves

---

### Timer and Game Flow
- Tracks total time spent solving a puzzle
- Displays elapsed time during gameplay
- Integrated cleanly into the main game loop

---

### Visual and UI Enhancements
- Color-coded terminal output for improved readability
- Bold formatting for original puzzle clues
- Visual distinction between user input, correct values, and incorrect entries
- Consistent and structured terminal layout

---

## Project Structure

├── boards/ # Pre-generated Sudoku boards by difficulty
├── solver/ # Backtracking solver implementation
├── validation/ # Sudoku rule enforcement and logic checks
├── game/ # Gameplay loop, menus, and UI logic
├── tests/ # Unit tests for core logic
├── main/ # Program entry point
└── README.md

yaml
Copy code

---

## Error Handling and Robustness

The system gracefully handles:
- Invalid inputs and malformed commands
- Attempts to modify protected cells
- Conflicting values and unsolvable boards
- Already-complete or invalid puzzle states

---

## Future Improvements

Potential future enhancements include:
- Support for larger board sizes (e.g., 16×16)
- Puzzle generation instead of pre-generated boards
- Difficulty rating based on solving complexity
- Graphical or web-based interface
- Performance optimizations for the solver

---

## License

This project is intended for personal and educational use.
