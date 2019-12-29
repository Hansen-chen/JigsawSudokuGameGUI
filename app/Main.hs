module Main where

import JigsawSudokuConstant
import JigsawSudokuType
import JigsawSudokuControl
import Data.Array
import Data.List
import System.IO
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main = loadGame "board/map.txt" >>= \g ->
            return GameState{game=g, currentCell=(0,0), solution=undefined} >>= \s ->
                windowDisplay >>= \w ->
                    play w white 100 s renderWorld inputHandler updateWorld

updateWorld :: Float -> GameState -> GameState
updateWorld _ state = state

renderWorld :: GameState -> Picture
renderWorld GameState{game=game, currentCell=cell} = renderGame game cell

renderGame :: Game -> (Int,Int) -> Picture
renderGame game cell = pictures 
  [
    renderBoard game,
    renderCurrentCell cell,
    renderMsg game
  ]

renderMsg :: Game -> Picture
renderMsg game = translate (-75) (-225) $ color black $ Scale 0.1 0.2 $ (Text $ message game) 
-- TODO: change with pictures including a rectangleSolid
-- rectangleSolid 360 (globalCellSize*3)

renderBoard :: Game -> Picture
renderBoard game = pictures
  [
    translate (((fromIntegral x) - 4) * globalCellSize) ((4 - (fromIntegral y)) * globalCellSize) $ renderCell cell ((getBlock $ board game) ! (x,y)) (blockColors game) | ((y, x), cell) <- assocs $ getNum $ board game
  ]

renderCell :: Int -> Int -> [Color] -> Picture
renderCell cell block blockColors = pictures 
  [
    color (blockColors !! block) $ rectangleSolid globalCellSize globalCellSize,
    --color black $ rectangleWire globalCellSize globalCellSize,
    translate (- globalCellSize / 4) (- globalCellSize / 4) $ scale 0.2 0.2 $ text $ if cell > 0 then show cell else "" 
  ]


renderCurrentCell :: (Int, Int) -> Picture
renderCurrentCell (x, y) =
  color black $ translate (((fromIntegral x) - 4) * globalCellSize) ((4 - (fromIntegral y)) * globalCellSize) $ rectangleWire globalCellSize globalCellSize
  

inputHandler :: Event -> GameState -> GameState
inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) state@(GameState{currentCell=cell}) = state{currentCell= moveCurrentCell cell (-1) 0}
inputHandler (EventKey (SpecialKey KeyRight) Down _ _) state@(GameState{currentCell=cell}) = state{currentCell= moveCurrentCell cell (1) 0}
inputHandler (EventKey (SpecialKey KeyDown) Down _ _) state@(GameState{currentCell=cell}) = state{currentCell= moveCurrentCell cell 0 (1)}
inputHandler (EventKey (SpecialKey KeyUp) Down _ _) state@(GameState{currentCell=cell}) = state{currentCell= moveCurrentCell cell 0 (-1) }
inputHandler (EventKey (Char c) Up _ _) state@(GameState{game=game, currentCell=cell, solution=solution})
  | '1' <= c && c <= '9' = -- Input
    state{game = move game cell (scanChar c)}
  | c == 'h' = -- Hint
    state{game = move game cell ((getNum solution) ! cell)}
  | c == 's' = -- Solve
    state{game = game{board = solution}}
  | otherwise = state

inputHandler _ s = s

moveCurrentCell :: (Int, Int) -> Int -> Int -> (Int, Int)
moveCurrentCell (x, y) dx dy | (x+dx)>=0 && (x+dx)<=8 && (y+dy)>=0 && (y+dy)<=8 = (x + dx, y + dy) 
                             | otherwise = (x,y)