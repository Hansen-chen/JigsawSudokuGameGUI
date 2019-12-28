module Main where

import JigsawSudokuConstant
import JigsawSudokuType
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main = display windowDisplay white (rectangleSolid globalCellSize globalCellSize)
