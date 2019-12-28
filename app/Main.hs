module Main where

import JigsawSudokuConstant
import JigsawSudokuType
import JigsawSudokuControl
import Data.Array
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
--    renderFocus cell,
    renderMsg game
  ]

renderMsg :: Game -> Picture
renderMsg game = translate (-75) (-225) $ color black $ Scale 0.1 0.2 $ (Text $ message game) 
-- TODO: change with pictures including a rectangleSolid
-- rectangleSolid 360 (globalCellSize*3)

renderBoard :: Game -> Picture
renderBoard game = pictures
  [
    translate (((fromIntegral x) - 4) * globalCellSize) ((4 - (fromIntegral y)) * globalCellSize) $ renderCell cell ((getBlock $ board game) ! (x,y)) | ((y, x), cell) <- assocs $ getNum $ board game
  ]

renderCell :: Int -> Int -> Picture
renderCell cell block = pictures 
  [
    -- TODO: change selectedColors to blockColors
    color (selectedColors !! block) $ rectangleSolid globalCellSize globalCellSize,
    color white $ rectangleWire globalCellSize globalCellSize,
    translate (- globalCellSize / 4) (- globalCellSize / 4) $ scale 0.2 0.2 $ text $ if cell > 0 then show cell else "" 
  ]

{--
renderFocus :: Coord -> Picture
renderFocus (r, c) =
  color black $ translate (((fromIntegral c) - 4) * globalCellSize) ((4 - (fromIntegral r)) * globalCellSize) $
    rectangleWire globalCellSize globalCellSize
--}  

inputHandler :: Event -> GameState -> GameState
inputHandler (EventKey (SpecialKey KeyUp) Down _ _) state@(GameState{currentCell=cell}) = state
inputHandler (EventKey (SpecialKey KeyDown) Down _ _) state@(GameState{currentCell=cell}) = state
inputHandler (EventKey (SpecialKey KeyRight) Down _ _) state@(GameState{currentCell=cell}) = state
inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) state@(GameState{currentCell=cell}) = state
inputHandler _ s = s
