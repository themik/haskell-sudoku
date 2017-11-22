module SudokuSolver where

import Sudoku

solve :: Sudoku.SudokuGrid -> Sudoku.SudokuGrid
solve grid = (bruteForce . fillDefinites) grid

fillDefinites :: Sudoku.SudokuGrid -> Sudoku.SudokuGrid
fillDefinites grid = grid

bruteForce :: Sudoku.SudokuGrid -> Sudoku.SudokuGrid
bruteForce grid = grid
