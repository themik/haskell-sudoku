module SudokuSolver where

import qualified Sudoku as S
import Data.Maybe(fromJust, isNothing)

solve :: S.Grid -> S.Grid
solve grid = (bruteForce . fillDefinites) grid

-- Fill out all the definite solutions
fillDefinites :: S.Grid -> S.Grid
fillDefinites grid = iter grid (fillDefinites' grid) where
  -- Iterate over rounds of fillDefinites' while a round decreases the number of empties
  iter :: S.Grid -> S.Grid -> S.Grid
  iter prv nxt | (S.countEmpties prv) > (S.countEmpties nxt) = iter nxt (fillDefinites' nxt)
               | otherwise                                   = nxt
  --
  fillDefinites' :: S.Grid -> S.Grid
  fillDefinites' grid = fillDefinites'' grid (S.nextEmptyPos grid Nothing)
  --
  fillDefinites'' :: S.Grid -> Maybe (Int, Int) -> S.Grid
  fillDefinites'' grid Nothing = grid
  fillDefinites'' grid pos     | (length vals) == 1 = fillDefinites'' newGrid nextPos
                               | otherwise          = fillDefinites'' grid nextPos where
    vals = S.getValidValsPosX grid (fromJust pos)
    nextPos = S.nextEmptyPos grid pos
    newGrid = S.setDefiniteVal grid (fromJust pos) (head vals)

-- Brute Force Depth First Search For a puzzle solution
bruteForce :: S.Grid -> S.Grid
bruteForce grid = fromJust (bruteForce' grid (S.nextEmptyPos grid Nothing))  where
  -- 
  bruteForce' :: S.Grid -> Maybe (Int, Int) -> Maybe S.Grid
  bruteForce' grid Nothing = Just grid
  bruteForce' grid pos = bruteForce'' grid pos (S.getValidValsPosX grid (fromJust pos)) 
  --
  bruteForce'' :: S.Grid -> Maybe (Int, Int) -> [Int] -> Maybe S.Grid
  bruteForce'' grid pos []   = Nothing
  bruteForce'' grid pos (x:xs) | isNothing newResult = bruteForce'' grid pos xs
                               | otherwise           = newResult where
    newResult = bruteForce' newGrid nextPos
    nextPos   = S.nextEmptyPos grid pos
    newGrid   = S.setGuessVal grid (fromJust pos) x
  
