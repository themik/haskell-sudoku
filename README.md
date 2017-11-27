# haskell-sudoku
This is a simple sudoku solver written in Haskell. It's the first toy project on my Haskell learning journey so don't use as an example of good coding style. I'll improve it as I learn more about the language.

## How to use

After cloning the repository, run via:

runghc SudokuMain.hs puzzles/sudoku01.txt

The first argument is the location of a text file containig a Sudoku puzzle. A valid puzzle file has 9 lines with 9 ascii digits each (no spaces).

## Included puzzles

The 50 included puzzles originate from [Project Euler problem 96](https://projecteuler.net/problem=96).

## Output

The puzzle solution along with some additional information is written to the standard outout. On consoles supporting ANSI escape sequences the puzzle solution output uses colours to distinguish between:
- green: cells whose value was derived unambiguously from row, column and quadrant in the initial puzzle. These cells are set in phase 1 of the solver.
- yellow: cells whose value was determined by the brute force solution search algorithm. These cells are set in phase 2 of the solver.
