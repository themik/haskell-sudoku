module SudokuSolver where

import qualified Sudoku as S
import Data.Maybe(fromJust)

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
    nextPos = (S.nextEmptyPos grid pos)
    newGrid = S.setDefiniteVal grid (fromJust pos) (head vals)

-- Brute Force Depth First Search For a puzzle solution
bruteForce :: S.Grid -> S.Grid
bruteForce grid = grid
