module JigsawSudokuConstant where

import Graphics.Gloss


globalCellSize :: Float
globalCellSize = 40

globalXOffset :: Float
globalXOffset = -300

globalYOffset :: Float
globalYOffset = -300

maxCellIndex :: Int
maxCellIndex = 24

windowDisplay :: Display
windowDisplay = InWindow "Window" (625, 625) (10, 10)