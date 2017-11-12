import Sudoku

main = do
       inpStr <- readFile "sudoku1.txt"
       let sudokuGrid = Sudoku.loadFromString inpStr
       putStrLn (show sudokuGrid)
       putStrLn ""
       putStrLn ("first row:" ++ (show (Sudoku.extractRow sudokuGrid 0)))
       putStrLn ("last row:" ++ (show (Sudoku.extractRow sudokuGrid 8)))
       putStrLn ""
       putStrLn ("first column:" ++ (show (Sudoku.extractColumn sudokuGrid 0)))
       putStrLn ("last column:" ++ (show (Sudoku.extractColumn sudokuGrid 8)))
       putStrLn ""
       putStrLn ("first quadrant:" ++ (show (Sudoku.extractQuadrant sudokuGrid (0, 0))))
       putStrLn ("last quadrant:" ++ (show (Sudoku.extractQuadrant sudokuGrid (8, 8))))
