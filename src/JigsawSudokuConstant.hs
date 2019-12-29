module JigsawSudokuConstant where

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.Environment (getScreenSize)
import Control.Monad

globalCellSize :: Float
globalCellSize = 40

windowWidth :: Int
windowWidth = 360+200

windowHeight :: Int
windowHeight = 450+200

selectedColors :: [Color]
selectedColors = map bright [cyan, magenta, rose, chartreuse, violet , aquamarine , azure, yellow, orange ]

windowDisplay :: IO Display
windowDisplay = getScreenSize >>= \(screenWidth, screenHeight) -> return (InWindow "Jigsaw Sudoku Game GUI" (windowWidth, windowHeight) (((screenWidth - windowWidth) `div` 2), ((screenHeight - windowHeight) `div` 2)))

