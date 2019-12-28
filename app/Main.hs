module Main where

import JigsawSudokuConstant
import JigsawSudokuType
import JigsawSudokuControl
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main = display windowDisplay white (rectangleSolid globalCellSize globalCellSize)

main = do
  (screenWidth, screenHeight) <- getScreenSize
  let 
    windowWidth = 360
    windowHeight = 450
    windowLeft = (screenWidth - windowWidth) `div` 2
    windowTop = (screenHeight - windowHeight) `div` 2
    window = InWindow "Jigsaw Sudoku" (windowWidth, windowHeight) (windowLeft, windowTop)

  game <- loadGame "maps/map.txt"
  let
    solution = solveGame game
    state = Playing{game=game, focus=(0,0), solution=solution}

  play window white 100 state renderWorld handleEvent updateWorld


updateWorld :: Float -> State -> State
updateWorld _ state = state

renderWorld :: State -> Picture
renderWorld Playing{game=game, focus=focus} = renderGame game focus


cellLength :: Float
cellLength = 40.0

renderGame :: Game -> Coord -> Picture
renderGame game focus = pictures 
  [
    renderBoard game,
    renderFocus focus,
    renderMsg
  ]

renderMsg :: Picture
renderMsg = translate (-75) (-200) $ color black $ Scale 0.1 0.2 $ (Text "Congratulations! You've won!") 
-- rectangleSolid 360 (cellLength*3)

renderBoard :: Game -> Picture
renderBoard game = pictures
  [
    translate (((fromIntegral c) - 4) * cellLength) ((4 - (fromIntegral r)) * cellLength) $ 
      renderCell cell ((blocks game) ! (r,c)) (elem (r, c) conflicts)
    | ((r, c), cell) <- assocs $ board game
  ]
  where
    conflicts = allConflicts game

renderCell :: Cell -> Int -> Bool -> Picture
renderCell cell block hasConflicts = pictures 
  [
    color (colorsOfBlocks !! block) $ rectangleSolid cellLength cellLength,
    color white $ rectangleWire cellLength cellLength,
    color (if hasConflicts then red else black) $ translate (- cellLength / 4) (- cellLength / 4) $ 
      scale 0.2 0.2 $ text $ maybe "" show cell
  ]

renderFocus :: Coord -> Picture
renderFocus (r, c) =
  color black $ translate (((fromIntegral c) - 4) * cellLength) ((4 - (fromIntegral r)) * cellLength) $
    rectangleWire cellLength cellLength


handleEvent :: Event -> State -> State

handleEvent (EventKey (SpecialKey KeyUp) Up _ _) state@(Playing{focus=focus}) = 
  state{focus = moveFocus focus (-1) 0}
handleEvent (EventKey (SpecialKey KeyDown) Up _ _) state@(Playing{focus=focus}) = 
  state{focus = moveFocus focus 1 0}
handleEvent (EventKey (SpecialKey KeyLeft) Up _ _) state@(Playing{focus=focus}) = 
  state{focus = moveFocus focus 0 (-1)}
handleEvent (EventKey (SpecialKey KeyRight) Up _ _) state@(Playing{focus=focus}) = 
  state{focus = moveFocus focus 0 1}

handleEvent (EventKey (SpecialKey k) Up _ _) state@(Playing{game=game, focus=focus}) 
  | elem k keys = -- Input
    state{game = makeMove game focus (fmap (+1) (elemIndex k keys))} 
  | k == KeyDelete || k == KeyBackspace = -- Erase
    state{game = makeMove game focus Nothing}
  | otherwise = state
  where
    keys = [KeyPad1, KeyPad2, KeyPad3, KeyPad4, KeyPad5, KeyPad6, KeyPad7, KeyPad8, KeyPad9]

handleEvent (EventKey (Char c) Up _ _) state@(Playing{game=game, focus=focus, solution=solution})
  | '1' <= c && c <= '9' = -- Input
    state{game = makeMove game focus (Just $ digitToInt c)}
  | c == '\b' = -- Erase
    state{game = makeMove game focus Nothing}
  | c == 'h' = -- Hint
    state{game = makeMove game focus (solution ! focus)}
  | c == 's' = -- Solve
    state{game = game{board = solution}}
  | otherwise = state

handleEvent _ state = state

moveFocus :: Coord -> Int -> Int -> Coord
moveFocus (r, c) dr dc =
  let 
    r' = r + dr
    c' = c + dc
  in
    if 0 <= r' && r' <= 8 && 0 <= c' && c' <= 8 
    then (r', c') else (r, c)
