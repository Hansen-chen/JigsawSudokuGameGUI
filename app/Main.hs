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
main = putStrLn "Please enter board file name under board folder of this project(exclude .txt): " >>= \_ ->
        getLine >>= \f ->
            loadGame f >>= \g ->
                    return GameState{game=g, currentCell=(0,0), solution=undefined, moves=[]} >>= \s ->
                        windowDisplay >>= \w ->
                            play w white 100 s renderUI inputHandler updateGame >>= \_ ->
                                putStrLn "Game ends."

updateGame :: Float -> GameState -> GameState
updateGame _ state = state

renderUI :: GameState -> Picture
renderUI GameState{game=game, currentCell=cell} = renderGame game cell

renderGame :: Game -> (Int,Int) -> Picture
renderGame game cell = pictures 
  [
    renderBoard game,
    renderCurrentCell cell,
    renderDecoration game
  ]

renderDecoration :: Game -> Picture
renderDecoration game = pictures 
    [
     translate (-globalCellSize*4) (globalCellSize*7) $ color black $ Scale 0.375 0.375 $ (Text $ "Jigsaw Sudoku"),
     translate (-globalCellSize*6.5) (globalCellSize*6) $ color black $ Scale 0.125 0.125 $ (Text $ "arrow(Up/Down/Left/Right): move current cell, 1-9: insert number"),
     translate (-globalCellSize*6.5) (globalCellSize*5.5) $ color black $ Scale 0.125 0.125 $ (Text $ "backspace: erase number, h: hint, a: solve Sudoku"),
     translate (-globalCellSize*6.5) (globalCellSize*5) $ color black $ Scale 0.125 0.125 $ (Text $ "u: undo, r: redo, s: save"),
     translate (-globalCellSize*4) (-globalCellSize*6) $ color black $ Scale 0.125 0.2 $ (Text $ message game),
     color blue $ translate (-globalCellSize*0) (-globalCellSize*6) $ rectangleWire (globalCellSize*9) (globalCellSize*3),
     color blue $ translate (-globalCellSize*0) (-globalCellSize*0) $ rectangleWire (globalCellSize*9) (globalCellSize*9)
    ]

renderBoard :: Game -> Picture
renderBoard game = pictures
  [
    translate (((fromIntegral x) - 4) * globalCellSize) ((4 - (fromIntegral y) ) * globalCellSize) $ renderCell cell ((getBlock $ board game) ! (x,y)) (blockColors game) (getNum (originalBoard game) ! (x,y)>0) | ((x, y), cell) <- assocs $ getNum $ board game
  ]

renderCell :: Int -> Int -> [Color] -> Bool -> Picture
renderCell cell block blockColors original= pictures 
  [
    color (blockColors !! block) $ rectangleSolid globalCellSize globalCellSize,
    color (if original then black else (blockColors !! block)) $ circle (globalCellSize*0.5),
    color black $ translate (- globalCellSize / 4) (- globalCellSize / 4) $ scale 0.22 0.22 $ text $ if cell > 0 then show cell else "" 
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
  | c == '\b' = -- Erase
    state{game = move game cell (-1)}
  | c == 'h' = -- Hint
    state{game = move game cell ((getNum solution) ! cell)}
  | c == 'a' = -- Solve
    state{game = game{board = solution}}
  | c =='s' = --Save
    state{game=saveGame game}
  | otherwise = state

inputHandler _ s = s

moveCurrentCell :: (Int, Int) -> Int -> Int -> (Int, Int)
moveCurrentCell (x, y) dx dy | (x+dx)>=0 && (x+dx)<=8 && (y+dy)>=0 && (y+dy)<=8 = (x + dx, y + dy) 
                             | otherwise = (x,y)