module Sudoku where

import Data.Char(digitToInt)
import Data.Maybe(fromJust)

-- Enum describing state of cells
data CellState = Empty | -- cell is currently empty
                 Fixed | -- non-empty cell part of the initial puzzle
                 Definite | -- non-empty cell whose value is known for sure
                 Guess -- value set as part of the solution algorithm
                       -- there is a chance it is wrong 

-- Representation of a Sudoku Grid
data Grid = Grid [[(Int , CellState)]]

-- Check the grid is 9x9 and only filled with 0 to 9s
verifySize :: Grid -> Bool
verifySize (Grid rows) = ((length rows) == 9) && (all verifyRow rows) where 
--
  verifyRow    row    = ((length row) == 9) && (all isSingleDigit row)
  isSingleDigit (x, _) = (x >= 0) && (x <= 9)


-- Loads a Sudoku Grid from a String. Input is one line per row
-- each containing 9 numeric characters
loadFromString :: String -> Grid
loadFromString input = Grid (map convertLine (lines input)) where
--
  convertLine line = map (digitToCell . digitToInt) line
  digitToCell x | x == 0    = (x, Empty)
                | otherwise = (x, Fixed)

-- Implemantation of type class Show for CellState
instance Show CellState where
  show Empty    = "empty"
  show Fixed    = "fixed"
  show Definite = "definite"
  show Guess    = "guess"

-- Equality type class for CellState
instance Eq CellState where
  Empty == Empty       = True
  Fixed == Fixed       = True
  Definite == Definite = True
  Guess == Guess       = True
  x == y               = False

-- Implementation of type class Show for Grid
instance Show Grid where
  show (Grid rows) = unlines (map printRow rows) where
--  
    printRow :: [(Int, CellState)] -> String
    printRow row    = foldl addFldToRow "" row    
    addFldToRow :: String -> (Int, CellState) -> String
    addFldToRow row fld | row == "" = showFld fld
                        | otherwise = row ++ " " ++ showFld fld
    showFld :: (Int, CellState) -> String
    showFld (x, Empty)    = "_" -- Empty fields displayed as underscore
    showFld (x, Fixed)    = show x -- For fixed fields just print the number
    showFld (x, Definite) = "\x1b[32m" ++ show x ++ "\x1b[0m" -- Definites in ANSI green
    showFld (x, Guess)    = "\x1b[33m" ++ show x ++ "\x1b[0m" -- Guesses in ANSI yellow

-- Count number of empty cells in grid
countEmpties :: Grid -> Int
countEmpties (Grid rows) = length (filter isEmpty (concat rows)) where
--
  isEmpty :: (Int, CellState) -> Bool
  isEmpty (_, Empty) = True
  isEmpty (_, _)     = False

-- Returns the state of the specified cell
getState :: Grid -> (Int, Int) -> CellState
getState grid pos = getStateFromCell (getCell grid pos)

-- Returns the numeric value of the specified cell
getVal :: Grid -> (Int, Int) -> Int
getVal grid pos = getValFromCell (getCell grid pos)

-- Get only the state from a cell (Int, CellState) tuple
getStateFromCell :: (Int, CellState) -> CellState
getStateFromCell (_, s) = s

-- Get only the value from a cell (Int, CellState) tuple
getValFromCell :: (Int, CellState) -> Int
getValFromCell (x, _) = x

-- Returns the value/State tuple from the specified cell
getCell :: Grid -> (Int, Int) -> (Int, CellState)
getCell (Grid rows) (rowIdx, colIdx) = extractPosFromRow colIdx (rows!!rowIdx)

-- Iterator returns the next position in the grid or Nothing if end is reached
-- Iteration can be started with Nothing
nextPos :: Maybe (Int, Int) -> Maybe (Int, Int)
nextPos Nothing       = Just (0, 0)
nextPos (Just (8, 8)) = Nothing
nextPos (Just (x, 8)) = Just (x + 1, 0)
nextPos (Just (x, y)) = Just (x, y + 1)

-- Iterator goes to the next empty position
nextEmptyPos :: Grid -> Maybe (Int, Int) -> Maybe (Int, Int)
nextEmptyPos grid pos | pos' == Nothing                           = Nothing
                      | (getState grid (fromJust pos'))  == Empty = pos'
                      | otherwise                                 = nextEmptyPos grid pos' where
  pos' = nextPos pos

-- Set Definite Value in a grid, returns the new Grid where the value is set
setDefiniteVal :: Grid -> (Int, Int) -> Int -> Grid
setDefiniteVal (Grid rows) (rowIdx, colIdx) val = Grid ((take rowIdx rows) ++ [(mdfRow (rows!!rowIdx) colIdx val Definite)] ++ (drop (rowIdx+1) rows))
 
-- Set Guess Value in a grid, returns the new Grid where the value is set
setGuessVal :: Grid -> (Int, Int) -> Int -> Grid
setGuessVal (Grid rows) (rowIdx, colIdx) val = Grid ((take rowIdx rows) ++ [(mdfRow (rows!!rowIdx) colIdx val Guess)] ++ (drop (rowIdx+1) rows))
 
mdfRow :: [(Int, CellState)] -> Int -> Int -> CellState -> [(Int, CellState)]
mdfRow row colIdx val state = (take colIdx row) ++ [(val, state)] ++ (drop (colIdx+1) row)

-- Extraction of a single row from a grid given a row number (excluding zeros)
extractRow :: Grid -> Int -> [Int]
extractRow grid rowIdx = filter0 (extractRow' grid rowIdx)

-- Extraction of a single row from a grid given a row number 
extractRow' :: Grid -> Int -> [Int]
extractRow' (Grid rows) rowIdx = map getValFromCell (rows!!rowIdx)

-- Extraction of the row from a grid minus the current cells value or zeros
extractRowX :: Grid -> (Int, Int) -> [Int]
extractRowX grid (rowIdx, colIdx) = filter0 (removeAt colIdx (extractRow' grid rowIdx))

-- Extraction of a single colum from a grid given a col number (excluding zeros)
extractColumn :: Grid -> Int -> [Int]
extractColumn grid colIdx = filter0 (extractColumn' grid colIdx)

-- Extraction of a single column from a grid given a col number
extractColumn' :: Grid -> Int -> [Int]
extractColumn' (Grid rows) colIdx = map (getValFromCell . extractPosFromRow colIdx) rows

extractPosFromRow :: Int -> [(Int, CellState)] -> (Int, CellState)
extractPosFromRow colIdx row = row!!colIdx

-- Extraction of a single column from a grid minus the current cells value or zeros 
extractColumnX :: Grid -> (Int, Int) -> [Int]
extractColumnX grid (rowIdx, colIdx) = filter0 (removeAt rowIdx (extractColumn' grid colIdx))

-- Extraction of a single quadrant from a grid given a row and col number (excluding zeros)
extractQuadrant :: Grid -> (Int, Int) -> [Int]
extractQuadrant grid pos = filter0 (extractQuadrant' grid pos) where

-- Extraction of a single quadrant from a grid given a row and col number
extractQuadrant' :: Grid -> (Int, Int) -> [Int]
extractQuadrant' (Grid rows) pos = extractQuadrantNorm rows (normalizePos pos) where
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
extractQuadrantX :: Grid -> (Int, Int) -> [Int]
extractQuadrantX grid pos = filter0 (removeAt (posToIdx pos) (extractQuadrant' grid pos)) where
--
  posToIdx :: (Int, Int) -> Int
  posToIdx (rowIdx, colIdx)  = (mod rowIdx 3) * 3 + (mod colIdx 3)

-- Check if the current value at the given coordinate satisfies row, column and quadrant constraints
isValidPos :: Grid -> (Int, Int) -> Bool
isValidPos grid (rowIdx, colIdx) = (isValidLine (extractRow grid rowIdx)) &&
                                (isValidLine (extractColumn grid colIdx)) &&
                                (isValidLine (extractQuadrant grid (rowIdx, colIdx)))

-- Check if setting the given value at the given coordinate is valid
isValidPosX :: Grid -> (Int, Int) -> Int -> Bool
isValidPosX grid pos val = (not (elem val (extractRowX grid pos))) &&
                           (not (elem val (extractColumnX grid pos))) &&
                           (not (elem val (extractQuadrantX grid pos))) 

-- Check if the entire grid satisfies all row, column and quadrant constraints
isValidGrid :: Grid -> Bool
isValidGrid grid = not (elem False ( 
                     map (isValidLine . (extractRow grid)) [0 .. 8] ++
                     map (isValidLine . (extractColumn grid)) [0 .. 8] ++
                     map (isValidLine . (extractQuadrant grid)) [(0,0) , (0,3), (0,6), (3,0), (3,3), (3,6), (6,0), (6,3), (6,6)]))


-- Check if a line (row, column or quadrant) is valid i.e. contains no duplicate numbers
isValidLine :: [Int] -> Bool
isValidLine [] = True
isValidLine (x:xs) | elem x xs = False -- invalid the same element appears at least twice in the list
                   | otherwise = isValidLine xs

-- Get all possible values for pos
getValidValsPosX :: Grid -> (Int, Int) -> [Int]
getValidValsPosX grid pos = filter (isValidPosX grid pos) [1 .. 9] 

-- Filter out all zeros
filter0 :: [Int] -> [Int]
filter0 = filter ((/=) 0)

-- Remove element with given index from list
removeAt :: Int -> [Int] -> [Int]
removeAt pos xs = start ++ (drop 1 end) where
   (start, end) = splitAt pos xs
