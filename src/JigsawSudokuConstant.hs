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
selectedColors = map (withAlpha 0.6) [cyan, magenta, rose, chartreuse, red, green, azure, blue, yellow]

windowDisplay :: IO Display
windowDisplay = getScreenSize >>= \(screenWidth, screenHeight) -> return (InWindow "Jigsaw Sudoku Game GUI" (windowWidth, windowHeight) (((screenWidth - windowWidth) `div` 2), ((screenHeight - windowHeight) `div` 2)))

