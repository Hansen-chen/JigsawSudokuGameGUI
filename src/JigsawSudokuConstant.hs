module JigsawSudokuConstant where

import Graphics.Gloss
import Graphics.Gloss.Data.Color

colorsOfBlocks :: [Color]
colorsOfBlocks = map (withAlpha 0.5)
  [
    cyan,
    magenta,
    rose,
    chartreuse,
    red,
    green,
    azure
    blue,
    yellow,
  ]


globalCellSize :: Float
globalCellSize = 40

maxCellIndex :: Int
maxCellIndex = 8

windowDisplay :: Display
windowDisplay = InWindow "Window" (625, 625) (10, 10)