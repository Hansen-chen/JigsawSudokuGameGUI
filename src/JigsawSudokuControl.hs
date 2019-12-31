module JigsawSudokuControl where

import JigsawSudokuType
import JigsawSudokuConstant
import JigsawSudokuGenerator
import Data.List
import Data.Array
import Data.Set (Set)
import System.IO
import System.IO.Unsafe
import System.Directory
import System.Random.Shuffle
import qualified Math.SetCover.Exact as ESC


loadGame :: String -> IO Game
loadGame f | notElem f (unsafePerformIO $ allTextsFiles) = generateBoard >>= \b -> writeFile ("board/"++f++".txt") (saveBoardFormat $ b) >>= \_ -> readFile ("board/"++f++".txt") >>= \gb -> shuffleM selectedColors >>= \c -> return (Game {board=(loadBoardFormat gb), originalBoard=(loadBoardFormat gb), message="Generate board "++f++" successfully!", filename=f, blockColors=c })
           | "-play" `isSuffixOf` f = readFile ("board/"++f++".txt") >>= \b -> readFile ("board/"++(take ((length f)-5) f)++".txt") >>= \ob -> shuffleM selectedColors >>= \c -> return (Game {board=(loadBoardFormat b), originalBoard=(loadBoardFormat ob), message="Read board "++(take ((length f)-5) f)++" successfully!", filename=(take ((length f)-5) f), blockColors=c }) 
           | otherwise = readFile ("board/"++f++".txt") >>= \b -> shuffleM selectedColors >>= \c -> return (Game {board=(loadBoardFormat b), originalBoard=(loadBoardFormat b), message="Read board "++f++" successfully!", filename=f, blockColors=c })

saveGame :: Game -> Game
saveGame game = unsafePerformIO $ writeFile ("board/"++(filename game)++"-play.txt") (saveBoardFormat $ board game) >>= \_ -> return (Game {board=(board game), message="Saved board "++(filename game)++" successfully!", blockColors=(blockColors game), originalBoard=(originalBoard game), filename=(filename game)})

-- Load Board String Manipulation
loadBoardFormat :: String -> Board
loadBoardFormat s = Board (array ((0,0),(8,8)) (arrayConstructor (map ((!!) b) [9..17]))) (array ((0,0),(8,8)) (arrayConstructor (map ((!!) b) [0..8])))
                    where b = map scanString (lines s)

-- Save Board String Manipulation                    
saveBoardFormat :: Board -> [Char]
saveBoardFormat (Board num loc) = filter (\x -> (x /=' ' && x/='\'')) (printArraySave loc ++ printArraySave num)

scanChar :: Char -> Int
scanChar c | '0' <= c && c <= '9' = fromEnum c - fromEnum '0'
           | otherwise = -1

scanString :: String -> [Int]
scanString [] = []
scanString (x:xs) | 0 <= sc && sc <= 9 = sc:(scanString xs)
                  | otherwise = -1:(scanString xs)
                    where sc = scanChar x

arrayConstructor :: [[Int]] -> [((Int, Int), Int)]
arrayConstructor [[]] = [((0,0),0)]
arrayConstructor m = [ ((x,y),m!!y!!x) | x <- [0..8], y <- [0..8] ]                   

printArraySave :: ( Array (Int,Int) Int ) -> [Char]
printArraySave arr = unlines [unwords [if (arr ! (x, y)) >= 0 then show (arr ! (x, y)) else show ('.') | x <- [0..8]] | y <- [0..8]]

-- Make Move with Jigsaw Sudoku game rules checking, insert move into array
move :: Game -> (Int, Int) -> Int -> Game
move game (x,y) n | x==(-99) && y==(-99) && n==(-99) && ((solveGame game) ! (0,0) == -1) = game{message ="This board has no solution!"}
                | x==(-99) && y==(-99) && n==(-99) && ((solveGame game) ! (0,0) /= -1) = game{board = (Board (solveGame game) (getBlock (board game))), message ="Solved the board! Press u to undo"}
                | x==(99) && y==(99) && n==(99) = game{board = (Board (getNum (originalBoard game)) (getBlock (board game))), message ="Cleared the board! Press u to undo"}
                | n == (-1) && ( getNum (originalBoard game) ! (x,y) <0) = game{board = (Board ((getNum (board game)) // [((x,y), n)]) (getBlock (board game))), message="Erased "++ (show ((getNum (board game)) ! (x,y))) ++ " in row " ++ (show y) ++ ", col " ++ (show x) }
                | (check (board game) x y n) && (getNum (originalBoard game) ! (x,y)<0) && (not $ jigsawSudokuCheck (Board ((getNum (board game)) // [((x,y), n)]) (getBlock (board game)))) = game{board = (Board ((getNum (board game)) // [((x,y), n)]) (getBlock (board game))), message="Inserted "++ (show n) ++ " in row " ++ (show y) ++ ", col " ++ (show x) }
                | (check (board game) x y n) && (getNum (originalBoard game) ! (x,y)<0) && (jigsawSudokuCheck (Board ((getNum (board game)) // [((x,y), n)]) (getBlock (board game)))) = game{board = (Board ((getNum (board game)) // [((x,y), n)]) (getBlock (board game))), message="Congratulations! You win the game!" }
                | n == (-1) && ( getNum (originalBoard game) ! (x,y) >0) = game{message="Cannot erase "++ (show ((getNum (board game)) ! (x,y))) ++ " in row " ++ (show y) ++ ", col " ++ (show x) }
                | otherwise = game{message="Cannot insert "++ (show n) ++ " in row " ++ (show y) ++ ", col " ++ (show x)}

-- Implement checking including the range of location(0-8), number to be inserted (1-9), the location is empty or not,
-- and the same line/row contains the number you are trying to insert or not.                            
check :: Board -> Int -> Int -> Int -> Bool
check (Board num loc) x y n = 0 <= x && x <= 8 && 0 <= y && y <= 8 && 1 <= n && n <= 9 && (num ! (x,y)) == -1
                                   && not (elem n (map (\((_,_),z) -> z) (filter (\((a,_),_) -> a == x) (assocs num))))
                                   && not (elem n (map (\((_,_),z) -> z) (filter (\((_,a),_) -> a == y) (assocs num))))
                                   && jigsawBlockCheck (Board num loc) x y n

-- The same Jigsaw Sudoku block does not contain the number you are trying to insert.
jigsawBlockCheck :: Board -> Int -> Int -> Int -> Bool                                  
jigsawBlockCheck (Board num loc) x y n = not (elem n (map ((!) num) (map (\((a,b),_) -> (a,b)) (filter (\((_,_),c) -> c == loc ! (x,y)) (assocs loc)))))

-- Decide whether the player win or not
-- TODO: win check includes Jigsaw Sudoku Game rules check
jigsawSudokuCheck :: Board -> Bool
jigsawSudokuCheck (Board num loc) = not (elem (-1) (elems num))

getNum :: Board -> (Array (Int, Int) Int)
getNum (Board num _) = num

getBlock :: Board -> (Array (Int, Int) Int)
getBlock (Board _ block) = block

jigsawSudokuGameUndo :: GameState -> GameState
jigsawSudokuGameUndo state | (gamePointer state) == 0 = state{game = (game state){message="End of Undo"}}
                           | otherwise = state{gamePointer = (gamePointer state)-1, game = jigsawSudokuGameReconstruct (game state){board = (initialBoard state), message="Initial Board"} (take ((gamePointer state)-1) (moves state)) }

jigsawSudokuGameRedo :: GameState -> GameState
jigsawSudokuGameRedo state | (gamePointer state) == length (moves state) = state{game = (game state){message="End of Redo"}}
                           | otherwise = state{gamePointer = (gamePointer state)+1, game = jigsawSudokuGameReconstruct (game state){board = (initialBoard state)} (take ((gamePointer state)+1) (moves state)) }

jigsawSudokuGameReconstruct :: Game -> [((Int,Int), Int)] -> Game
jigsawSudokuGameReconstruct game [] = game
jigsawSudokuGameReconstruct game (x:xs) = jigsawSudokuGameReconstruct (move game (fst x) (snd x)) xs

solveGame :: Game -> (Array (Int, Int) (Int))
solveGame game =
   let
      initAssigns =  assigns $ getBlock (originalBoard game)
      occupied = filter (\((_,_),i) -> i /= (-1)) $ assocs $ getNum (originalBoard game)
      occupiedAssigns = [assign n r c b | ((r, c), n) <- occupied, let b = (getBlock (originalBoard game)) ! (r, c)]
      solution = getSol initAssigns occupiedAssigns
   in
      array ((0, 0), (8, 8)) [((r, c), n) | ((r, c), n) <- solution]
 
allTextsFiles :: IO [String]
allTextsFiles = getDirectoryContents "board/" >>= \files -> return [ take (length x -4) x | x <- files, (length x) > 4 ]
 
getSol :: [Assign (Set X)] -> [Assign (Set X)] -> [((Int, Int), (Int))]
getSol initAssigns occupiedAssigns | (length (ESC.search $ foldl (flip ESC.updateState) (ESC.initState initAssigns) occupiedAssigns)) == 0 =[ ((x,y), -1) | x <- [0..8], y <- [0..8]]
                                   | otherwise = (head $ ESC.search $ foldl (flip ESC.updateState) (ESC.initState initAssigns) occupiedAssigns)


