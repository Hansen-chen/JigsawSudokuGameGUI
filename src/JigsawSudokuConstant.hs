module JigsawSudokuConstant where

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Control.Monad
import Data.Array

-- Cell size in board in GUI
globalCellSize :: Float
globalCellSize = 40

-- Block colors in GUI
selectedColors :: [Color]
selectedColors = map bright [cyan, magenta, rose, chartreuse, violet , aquamarine , azure, yellow, orange ]

-- Regular Blocks Array
regularBlocks :: (Array (Int, Int) Int)
regularBlocks = array ((0, 0), (8, 8)) ([((x, y), (x `div` 3) * 3 + (y `div` 3)) | x <- [0..8], y <- [0..8]])
