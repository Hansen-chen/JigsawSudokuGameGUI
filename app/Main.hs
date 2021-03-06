module Main where

import JigsawSudokuConstant
import JigsawSudokuType
import JigsawSudokuControl
import Data.Array
import Data.List
import System.IO
import System.Directory
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- Main entry of the game before enter GUI

main :: IO ()
main = chooseBoard "start" >>= \f ->
                if (f == "-1") 
                then putStrLn "Bye"
                else
                  loadGame f >>= \g ->
                    putStrLn "Loading Jigsaw Sudoku Game board ..." >>= \_ ->
                      putStrLn "Entering Jigsaw Sudoku Game GUI..." >>= \_ ->
                        return GameState{game=g, currentCell=(0,0), solution=(Board (solveGame g) (getBlock $ board g)), initialBoard=(board g),moves=[], gamePointer=0} >>= \s ->
                            play FullScreen white 100 s renderUI inputHandler updateGame

-- Decoration in main entry of the game before enter GUI

chooseDecoration :: IO ()
chooseDecoration = putStrLn "******************************************" >>= \_ ->
                   putStrLn "* Before Entering Jigsaw Sudoku Game GUI *" >>= \_ ->
                   putStrLn "******************************************"

-- Get all available board and display in main entry of the game before enter GUI

chooseBoard :: String -> IO String
chooseBoard state | state == "start" =  chooseDecoration >>= \_ ->putStrLn "\nAvailable Boards:\n" >>= \_ ->
                                          showBoards >>= \a ->
                                            mapM putStrLn a >>= \_ ->
                                              putStrLn "\nPlease enter board number to enter GUI or -1 to quit, then press enter" >>= \_ ->
                                                getLine >>= \f ->
                                                  if (isInteger f)
                                                    then
                                                     if (f == "-1") then return "-1"
                                                     else
                                                       return (read f :: Int) >>= \number ->
                                                         if(number > (length a)-1 || number <0)
                                                           then
                                                             putStrLn "Incorrect Input ! Please input number again." >>= \_ -> chooseBoard "start again"
                                                          else
                                                            if (number == (length a -1))
                                                              then
                                                                putStrLn "Please input board name (no more than 10 characters) and press enter" >>= \_ ->
                                                                  putStrLn "Note: characters in name which is not alphabet or digit will be removed" >>= \_ ->
                                                                    getLine >>= \newF ->
                                                                      putStrLn "Generating new Jigsaw Sudoku Game board ..." >>= \_ ->
                                                                        return (take 10 (validNewBoardName newF))
                                                            else
                                                              allBoards >>= \boardModule ->
                                                                allTextsFiles >>= \boardFile ->
                                                                  if(notElem (boardModule !! number ++ "-play") boardFile)
                                                                    then
                                                                      return (boardModule !! number)
                                                                  else
                                                                    return (boardModule !! number ++ "-play")
                                                                

                                                            
                                                   else putStrLn "Incorrect Input ! Please input number again." >>= \_ -> chooseBoard "start again"
                  | otherwise = (chooseBoard "start")

-- Display all available board options
        
showBoards :: IO [String]
showBoards = getDirectoryContents "board/" >>= \files -> 
  return [ take (length x -4) x | x <- files, (length x) > 4 && not ("-play.txt" `isSuffixOf` x) ] >>= \boards ->
    return (zip [0..(length boards -1)] boards) >>= \options ->
      return ([ (show (fst o)) ++ " => " ++ (snd o)  | o <- options] ++ [(show (length options)) ++ " => Random Board Generation"])

-- Get all available board raw data

allBoards :: IO [String]
allBoards = getDirectoryContents "board/" >>= \files -> return [ take (length x -4) x | x <- files, (length x) > 4 && not ("-play.txt" `isSuffixOf` x) ]

-- Check whether a String can be converted to integer

isInteger :: String -> Bool
isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False

-- Update GameState in GUI, it is not used but necessary for rendering GUI 

updateGame :: Float -> GameState -> GameState
updateGame _ state = state

-- Render whole UI to GUI

renderUI :: GameState -> Picture
renderUI GameState{game=game, currentCell=cell} = renderGame game cell

-- Render whole Game to UI

renderGame :: Game -> (Int,Int) -> Picture
renderGame game cell = pictures 
  [
    renderBoard game,
    renderCurrentCell cell,
    renderDecoration game
  ]

-- Render decorations to UI

renderDecoration :: Game -> Picture
renderDecoration game = pictures 
    [
     translate (-globalCellSize*4) (globalCellSize*7) $ color black $ Scale 0.375 0.375 $ (Text $ "Jigsaw Sudoku"),
     translate (-globalCellSize*6.5) (globalCellSize*6) $ color black $ Scale 0.125 0.125 $ (Text $ "arrow(Up/Down/Left/Right): move current cell, 1-9: insert number"),
     translate (-globalCellSize*6.5) (globalCellSize*5.5) $ color black $ Scale 0.125 0.125 $ (Text $ "backspace/delete: erase number, h: hint, a: solve Sudoku board"),
     translate (-globalCellSize*6.5) (globalCellSize*5) $ color black $ Scale 0.125 0.125 $ (Text $ "c: clear Sudoku board, u: undo, r: redo, s: save, Esc: quit"),
     translate (-globalCellSize*4) (-globalCellSize*6) $ color black $ Scale 0.125 0.2 $ (Text $ message game),
     color blue $ translate (-globalCellSize*0) (-globalCellSize*6) $ rectangleWire (globalCellSize*9) (globalCellSize*3),
     color blue $ translate (-globalCellSize*0) (-globalCellSize*0) $ rectangleWire (globalCellSize*9) (globalCellSize*9)
    ]

-- Render Game Board to UI

renderBoard :: Game -> Picture
renderBoard game = pictures
  [
    translate (((fromIntegral x) - 4) * globalCellSize) ((4 - (fromIntegral y) ) * globalCellSize) $ renderCell cell ((getBlock $ board game) ! (x,y)) (blockColors game) (getNum (originalBoard game) ! (x,y)>0) | ((x, y), cell) <- assocs $ getNum $ board game
  ]

-- Render Cell to Game Board

renderCell :: Int -> Int -> [Color] -> Bool -> Picture
renderCell cell block blockColors original= pictures 
  [
    color (blockColors !! block) $ rectangleSolid globalCellSize globalCellSize,
    color (if original then black else (blockColors !! block)) $ circle (globalCellSize*0.5),
    color black $ translate (- globalCellSize / 4) (- globalCellSize / 4) $ scale 0.22 0.22 $ text $ if cell > 0 then show cell else "" 
  ]

-- Render current cell to Game Board

renderCurrentCell :: (Int, Int) -> Picture
renderCurrentCell (x, y) =
  color black $ translate (((fromIntegral x) - 4) * globalCellSize) ((4 - (fromIntegral y)) * globalCellSize) $ rectangleWire globalCellSize globalCellSize

-- Handle all actions performed by player by pressing keys in keyboard  

inputHandler :: Event -> GameState -> GameState
inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) state@(GameState{currentCell=cell}) = state{currentCell= moveCurrentCell cell (-1) 0}
inputHandler (EventKey (SpecialKey KeyRight) Down _ _) state@(GameState{currentCell=cell}) = state{currentCell= moveCurrentCell cell (1) 0}
inputHandler (EventKey (SpecialKey KeyDown) Down _ _) state@(GameState{currentCell=cell}) = state{currentCell= moveCurrentCell cell 0 (1)}
inputHandler (EventKey (SpecialKey KeyUp) Down _ _) state@(GameState{currentCell=cell}) = state{currentCell= moveCurrentCell cell 0 (-1) }
inputHandler (EventKey (SpecialKey KeyDelete) Down _ _) state@(GameState{game=game, currentCell=cell, solution=solution, moves=moves, gamePointer=pointer}) = state{game = move game cell (-1),moves=(movesUpdate moves (cell, (-1)) pointer), gamePointer=pointer+1}
inputHandler (EventKey (SpecialKey KeyBackspace) Down _ _) state@(GameState{game=game, currentCell=cell, solution=solution, moves=moves, gamePointer=pointer}) = state{game = move game cell (-1),moves=(movesUpdate moves (cell, (-1)) pointer), gamePointer=pointer+1}

inputHandler (EventKey (Char c) Up _ _) state@(GameState{game=game, currentCell=cell, solution=solution, moves=moves, gamePointer=pointer})
  | '1' <= c && c <= '9' = -- Input
    state{game = move game cell (scanChar c),moves=(movesUpdate moves (cell, (scanChar c)) pointer), gamePointer=pointer+1} 
  | c == '\b' = -- Erase
    state{game = move game cell (-1),moves=(movesUpdate moves (cell, (-1)) pointer), gamePointer=pointer+1}
  | c == 'h' = -- Hint
    state{game = game{message = (generateHint solution cell)}}
  | c == 'a' = -- Solve
    state{game = move game (-99,-99) (-99), moves=(movesUpdate moves (((-99),(-99)), (-99)) pointer), gamePointer=pointer+1}
  | c == 'c' = -- Clear board
    state{game = move game (99,99) (99), moves=(movesUpdate moves (((99),(99)), (99)) pointer), gamePointer=pointer+1}
  | c =='s' = --Save
    state{game=saveGame game}
  | c =='r' = --Redo
    jigsawSudokuGameRedo state
  | c =='u' = --Undo
    jigsawSudokuGameUndo state
  | otherwise = state

inputHandler _ s = s

-- Move current cell

moveCurrentCell :: (Int, Int) -> Int -> Int -> (Int, Int)
moveCurrentCell (x, y) dx dy | (x+dx)>=0 && (x+dx)<=8 && (y+dy)>=0 && (y+dy)<=8 = (x + dx, y + dy) 
                             | otherwise = (x,y)

-- Moves list update

movesUpdate :: [((Int, Int), Int)] -> ((Int, Int), Int) -> Int -> [((Int, Int), Int)]
movesUpdate oldMoves newMove pointer | (length oldMoves) == 0  = [newMove]
                                     | pointer == (length oldMoves) && (length oldMoves)>0  = oldMoves++[newMove]
                                     | pointer < (length oldMoves) = (take pointer oldMoves)++[newMove]
                                     | otherwise = oldMoves

-- Check whether current board name is valid or not

validNewBoardName :: String -> String
validNewBoardName n | (filter (\xs -> (xs /=' ') && (isLetterOrDigit xs)) n) == "" = "unname"
                    | (filter (\xs -> (xs /=' ') && (isLetterOrDigit xs)) n) == n = n
                    | otherwise = (filter (\xs -> (xs /=' ') && (isLetterOrDigit xs)) n)

-- Check whether a char is letter/digit or not

isLetterOrDigit :: Char -> Bool
isLetterOrDigit c = (('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')|| ('0' <= c && c <= '9'))

-- Generate hint string for message box

generateHint :: Board -> (Int, Int) ->String
generateHint solution cell | ((getNum solution) ! cell) == -1 = "Hint: This board has no solution"
                           | otherwise = "Hint: " ++ (show $ (getNum solution) ! cell) ++ " is in row " ++ (show $ snd cell) ++ " col " ++ (show $ fst cell)  
