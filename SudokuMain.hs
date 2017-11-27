import qualified Sudoku as S
import qualified SudokuSolver as Solver
import System.Environment

main = do
       args <- System.Environment.getArgs
       if length args /= 1 
         then do
           putStrLn "Please provide the path to a Sudoku puzzle file as argument."
         else do
           inpStr <- readFile (head args)
           let grid = S.loadFromString inpStr
           putStrLn (show grid)
           if not (S.verifySize grid) then
             putStrLn "Cannot solve. Grid size not 9x9."
           else do
             putStrLn "Grid size verified."
             if not (S.isValidGrid grid) then
               putStrLn "Aborting. Initial grid violates Sudoku puzzle constraints."
             else do
               putStrLn "Initial grid puzzle constraints verified."
               putStrLn ("Solving puzzle...")
               putStrLn ("Number of empty cells " ++ show (S.countEmpties grid))
               putStrLn ""
               let solvedGrid = Solver.solve grid
               putStrLn (show solvedGrid)  
               putStrLn ("Number of empty cells " ++ show (S.countEmpties solvedGrid))
               if (S.isValidGrid grid) then
                 putStrLn "Solution verified."
               else do
                 putStrLn "Something went terrible wrong. Solution violates Sudoku puzzle constraints."
