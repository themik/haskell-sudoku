import Sudoku
import SudokuSolver
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
           let solvedGrid = SudokuSolver.solve sudokuGrid
           putStrLn (show solvedGrid)  
