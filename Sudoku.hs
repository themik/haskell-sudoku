module Sudoku where

import Data.Char(digitToInt)

-- Enum describing state of cells
data CellState = Empty | -- cell is currently empty
                 Fixed | -- non-empty cell part of the initial puzzle
                 Definite | -- non-empty cell whose value is known for sure
                 Guess -- value set as part of the solution algorithm
                       -- there is a chance it is wrong 

data SudokuGrid = SudokuGrid [[(Int , CellState)]]

-- Check the grid is 9x9 and only filled with 0 to 9s
verifySize :: SudokuGrid -> Bool
verifySize (SudokuGrid rows) = ((length rows) == 9) && (all verifyRow rows) where 
--
  verifyRow    row    = ((length row) == 9) && (all isSingleDigit row)
  isSingleDigit (x, _) = (x >= 0) && (x <= 9)


-- Loads a Sudoku Grid from a String. Input is one line per row
-- each containing 9 numeric characters
loadFromString :: String -> SudokuGrid
loadFromString input = SudokuGrid (map convertLine (lines input)) where
--
  convertLine line = map (digitToCell . digitToInt) line
  digitToCell x | x == 0    = (x, Empty)
                | otherwise = (x, Fixed)


instance Show CellState where
  show Empty    = "empty"
  show Fixed    = "fixed"
  show Definite = "definite"
  show Guess    = "guess"

-- Implementation of type class Show
instance Show SudokuGrid where
  show (SudokuGrid rows) = unlines (map printRow rows) where
--  
    printRow :: [(Int, CellState)] -> String
    printRow row    = foldl addFldToRow "" row    
    addFldToRow :: String -> (Int, CellState) -> String
    addFldToRow row fld | row == "" = showFld fld
                        | otherwise = row ++ " " ++ showFld fld
    showFld :: (Int, CellState) -> String
    showFld (x, _) | x == 0  = "_"
                   | otherwise = show x

-- Extraction of a single row from a grid given a row number (excluding zeros)
extractRow :: SudokuGrid -> Int -> [Int]
extractRow grid rowIdx = filter0 (extractRow' grid rowIdx)

-- Extraction of a single row from a grid given a row number 
extractRow' :: SudokuGrid -> Int -> [Int]
extractRow' (SudokuGrid rows) rowIdx = map getValFromCell (rows!!rowIdx)

-- Extraction of the row from a grid minus the current cells value or zeros
extractRowX :: SudokuGrid -> (Int, Int) -> [Int]
extractRowX grid (rowIdx, colIdx) = filter0 (removeAt colIdx (extractRow' grid rowIdx))

-- Extraction of a single colum from a grid given a col number (excluding zeros)
extractColumn :: SudokuGrid -> Int -> [Int]
extractColumn grid colIdx = filter0 (extractColumn' grid colIdx)

-- Extraction of a single column from a grid given a col number
extractColumn' :: SudokuGrid -> Int -> [Int]
extractColumn' (SudokuGrid rows) colIdx = map (extractPos colIdx) rows where
--
  extractPos :: Int -> [(Int, CellState)] -> Int
  extractPos colIdx row = getValFromCell (row!!colIdx)

-- Extraction of a single column from a grid minus the current cells value or zeros 
extractColumnX :: SudokuGrid -> (Int, Int) -> [Int]
extractColumnX grid (rowIdx, colIdx) = filter0 (removeAt rowIdx (extractColumn' grid colIdx))

-- Extraction of a single quadrant from a grid given a row and col number (excluding zeros)
extractQuadrant :: SudokuGrid -> (Int, Int) -> [Int]
extractQuadrant grid pos = filter0 (extractQuadrant' grid pos) where

-- Extraction of a single quadrant from a grid given a row and col number
extractQuadrant' :: SudokuGrid -> (Int, Int) -> [Int]
extractQuadrant' (SudokuGrid rows) pos = extractQuadrantNorm rows (normalizePos pos) where
--
  normalizePos :: (Int, Int) -> (Int, Int)
  normalizePos (rowIdx, colIdx) = (normalizeIdx rowIdx, normalizeIdx colIdx)
  normalizeIdx :: Int -> Int
  normalizeIdx idx = idx - mod idx 3
  extractQuadrantNorm :: [[(Int, CellState)]] -> (Int, Int) -> [Int]
  extractQuadrantNorm rows (rowIdx, colIdx) = (extractSubRow (rows!!rowIdx) colIdx) ++ 
                                              (extractSubRow (rows!!(rowIdx+1)) colIdx) ++ 
                                              (extractSubRow (rows!!(rowIdx+2)) colIdx)
  extractSubRow :: [(Int, CellState)] -> Int -> [Int]
  extractSubRow row colIdx = map getValFromCell (take 3 . drop colIdx $ row)

-- Extraction of a single quadrant from a grid given a row and col number minus the current cells value or zeros
extractQuadrantX :: SudokuGrid -> (Int, Int) -> [Int]
extractQuadrantX grid pos = filter0 (removeAt (posToIdx pos) (extractQuadrant' grid pos)) where
--
  posToIdx :: (Int, Int) -> Int
  posToIdx (rowIdx, colIdx)  = (mod rowIdx 3) * 3 + (mod colIdx 3)

-- Get only the value from a cell (Int, CellState) tuple
getValFromCell :: (Int, CellState) -> Int
getValFromCell (x, _) = x

-- Check if the current value at the given coordinate satisfies row, column and quadrant constraints
isValidPos :: SudokuGrid -> (Int, Int) -> Bool
isValidPos grid (rowIdx, colIdx) = (isValidLine (extractRow grid rowIdx)) &&
                                (isValidLine (extractColumn grid colIdx)) &&
                                (isValidLine (extractQuadrant grid (rowIdx, colIdx)))

-- Check if setting the given value at the given coordinate is valid
isValidPosX :: SudokuGrid -> Int -> (Int, Int) -> Bool
isValidPosX grid val pos = (not (elem val (extractRowX grid pos))) &&
                          (not (elem val (extractColumnX grid pos))) &&
                          (not (elem val (extractQuadrantX grid pos))) 


-- Check if a line (row, column or quadrant) is valid i.e. contains no duplicate numbers
isValidLine :: [Int] -> Bool
isValidLine [] = True
isValidLine (x:xs) | elem x xs = False -- invalid the same element appears at least twice in the list
                   | otherwise = isValidLine xs

-- Filter out all zeros
filter0 :: [Int] -> [Int]
filter0 = filter ((/=) 0)

-- Remove element with given index from list
removeAt :: Int -> [Int] -> [Int]
removeAt pos xs = start ++ (drop 1 end) where
   (start, end) = splitAt pos xs
