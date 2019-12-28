module JigsawSudokuConstant where

import Graphics.Gloss


globalCellSize :: Float
globalCellSize = 40

maxCellIndex :: Int
maxCellIndex = 8

windowDisplay :: Display
windowDisplay = InWindow "Window" (625, 625) (10, 10)