import Sudoku
import System.Environment

main = do
       args <- System.Environment.getArgs
       if length args /= 1 
         then do
           putStrLn "Please provide the path to a Sudoku puzzle files as argument."
         else do
           inpStr <- readFile "sudoku1.txt"
           let sudokuGrid = Sudoku.loadFromString inpStr
           putStrLn (show sudokuGrid)
           putStrLn ("Solving puzzle...")
           putStrLn ""
           let solvedGrid = solve sudokuGrid
           putStrLn (show solvedGrid)  


solve :: Sudoku.SudokuGrid -> Sudoku.SudokuGrid
solve grid = grid


