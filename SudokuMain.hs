import qualified Sudoku as S
import qualified SudokuSolver as Solver
import System.Environment

main = do
       args <- System.Environment.getArgs
       if length args /= 1 
         then do
           putStrLn "Please provide the path to a Sudoku puzzle file as argument."
         else do
           inpStr <- readFile "sudoku1.txt"
           let grid = S.loadFromString inpStr
           putStrLn (show grid)
           putStrLn ("Solving puzzle...")
           putStrLn ("Number of empty cells " ++ show (S.countEmpties grid))
           putStrLn ""
           let solvedGrid = Solver.solve grid
           putStrLn (show solvedGrid)  
           putStrLn ("Number of empty cells " ++ show (S.countEmpties solvedGrid))
