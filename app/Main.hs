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

main :: IO ()
main = chooseBoard "start" >>= \f ->
                if (f == "-1") 
                then putStrLn "Bye"
                else
                  loadGame f >>= \g ->
                    putStrLn "Please enjoy the game!" >>= \_ ->
                      return GameState{game=g, currentCell=(0,0), solution=(Board (solveGame g) (getBlock $ board g)), initialBoard=(board g),moves=[], gamePointer=0} >>= \s ->
                          play FullScreen white 100 s renderUI inputHandler updateGame

chooseDecoration :: IO ()
chooseDecoration = putStrLn "******************************************" >>= \_ ->
                   putStrLn "* Before Entering Jigsaw Sudoku Game GUI *" >>= \_ ->
                   putStrLn "******************************************"

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
                                                            -- random board generation
                                                            allBoards >>= \boardModule ->
                                                              allTextsFiles >>= \boardFile ->
                                                                if(notElem (boardModule !! number ++ "-play") boardFile)
                                                                  then
                                                                    return (boardModule !! number)
                                                                else
                                                                  return (boardModule !! number ++ "-play")
                                                                

                                                            
                                                   else putStrLn "Incorrect Input ! Please input number again." >>= \_ -> chooseBoard "start again"
                  | otherwise = (chooseBoard "start")


        
showBoards :: IO [String]
showBoards = getDirectoryContents "board/" >>= \files -> 
  return [ take (length x -4) x | x <- files, (length x) > 4 && not ("-play.txt" `isSuffixOf` x) ] >>= \boards ->
    return (zip [0..(length boards -1)] boards) >>= \options ->
      return ([ (show (fst o)) ++ " => " ++ (snd o)  | o <- options] ++ [(show (length options)) ++ " => Random Board Generation"])

allBoards :: IO [String]
allBoards = getDirectoryContents "board/" >>= \files -> return [ take (length x -4) x | x <- files, (length x) > 4 && not ("-play.txt" `isSuffixOf` x) ]

allTextsFiles :: IO [String]
allTextsFiles = getDirectoryContents "board/" >>= \files -> return [ take (length x -4) x | x <- files, (length x) > 4 ]

isInteger :: String -> Bool
isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False

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
     translate (-globalCellSize*6.5) (globalCellSize*5.5) $ color black $ Scale 0.125 0.125 $ (Text $ "backspace/delete: erase number, h: hint, a: solve Sudoku"),
     translate (-globalCellSize*6.5) (globalCellSize*5) $ color black $ Scale 0.125 0.125 $ (Text $ "u: undo, r: redo, s: save, Esc: quit"),
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
inputHandler (EventKey (SpecialKey KeyDelete) Down _ _) state@(GameState{game=game, currentCell=cell, solution=solution, moves=moves, gamePointer=pointer}) = state{game = move game cell (-1),moves=(movesUpdate moves (cell, (-1)) pointer), gamePointer=pointer+1}
inputHandler (EventKey (SpecialKey KeyBackspace) Down _ _) state@(GameState{game=game, currentCell=cell, solution=solution, moves=moves, gamePointer=pointer}) = state{game = move game cell (-1),moves=(movesUpdate moves (cell, (-1)) pointer), gamePointer=pointer+1}

inputHandler (EventKey (Char c) Up _ _) state@(GameState{game=game, currentCell=cell, solution=solution, moves=moves, gamePointer=pointer})
  | '1' <= c && c <= '9' = -- Input
    state{game = move game cell (scanChar c),moves=(movesUpdate moves (cell, (scanChar c)) pointer), gamePointer=pointer+1} 
  | c == '\b' = -- Erase
    state{game = move game cell (-1),moves=(movesUpdate moves (cell, (-1)) pointer), gamePointer=pointer+1}
  | c == 'h' = -- Hint
    state{game = game{message = "Hint: " ++ (show $ (getNum solution) ! cell) ++ " is in row " ++ (show $ snd cell) ++ " col " ++ (show $ fst cell)  }}
  | c == 'a' = -- Solve
    state{game = move game (-99,-99) (-99), moves=(movesUpdate moves (((-99),(-99)), (-99)) pointer), gamePointer=pointer+1}
  | c =='s' = --Save
    state{game=saveGame game}
  | c =='r' = --Redo
    jigsawSudokuGameRedo state
  | c =='u' = --Undo
    jigsawSudokuGameUndo state
  | otherwise = state

inputHandler _ s = s

moveCurrentCell :: (Int, Int) -> Int -> Int -> (Int, Int)
moveCurrentCell (x, y) dx dy | (x+dx)>=0 && (x+dx)<=8 && (y+dy)>=0 && (y+dy)<=8 = (x + dx, y + dy) 
                             | otherwise = (x,y)

movesUpdate :: [((Int, Int), Int)] -> ((Int, Int), Int) -> Int -> [((Int, Int), Int)]
movesUpdate oldMoves newMove pointer | (length oldMoves) == 0  = [newMove]
                                     | pointer == (length oldMoves) && (length oldMoves)>0  = oldMoves++[newMove]
                                     | pointer < (length oldMoves) = (take pointer oldMoves)++[newMove]
                                     | otherwise = oldMoves

