module JigsawSudokuType where
   
import Graphics.Gloss
import Data.Array

-- First matrix records number in the Board, -1 as not filled
-- The second matrix records the board location
data Board = Board (Array (Int, Int) Int) (Array (Int, Int) Int) deriving (Eq)


