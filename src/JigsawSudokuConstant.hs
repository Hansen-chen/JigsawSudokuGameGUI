module JigsawSudokuConstant where

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Control.Monad

globalCellSize :: Float
globalCellSize = 40

selectedColors :: [Color]
selectedColors = map bright [cyan, magenta, rose, chartreuse, violet , aquamarine , azure, yellow, orange ]

