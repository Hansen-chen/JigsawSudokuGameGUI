module JigsawSudokuConstant where

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.Environment (getScreenSize)
import System.Random
import Data.Array.IO
import Control.Monad

-- | Randomly shuffle a list
--   /O(N)/
-- https://wiki.haskell.org/Random_shuffle
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs



globalCellSize :: Float
globalCellSize = 40

maxCellIndex :: Int
maxCellIndex = 8

windowWidth :: Int
windowWidth = 360

windowHeight :: Int
windowHeight = 450

selectedColors :: [Color]
selectedColors = map (withAlpha 0.6) [cyan, magenta, rose, chartreuse, red, green, azure, blue, yellow]

windowDisplay :: IO Display
windowDisplay = getScreenSize >>= \(screenWidth, screenHeight) -> return (InWindow "Jigsaw Sudoku Game GUI" (windowWidth, windowHeight) (((screenWidth - windowWidth) `div` 2), ((screenHeight - windowHeight) `div` 2)))

blockColors :: IO [Color]
blockColors = shuffle selectedColors

