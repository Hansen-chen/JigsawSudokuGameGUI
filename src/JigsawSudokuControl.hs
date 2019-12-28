module JigsawSudokuControl where

import JigsawSudokuType
import JigsawSudokuConstant
import Data.Array
import System.IO
import System.Random.Shuffle

loadGame :: String -> IO Game
loadGame f = readFile f >>= \b -> shuffleM selectedColors >>= \c -> return (Game {board=(loadBoardFormat b), message="Read board successfully!", instruction="l: load", blockColors=c })

saveGame :: Game -> String -> IO Game
saveGame game f = writeFile f (saveBoardFormat $ board game) >>= \_ -> return (Game {board=(board game), message="Save!", instruction=(instruction game), blockColors=(blockColors game) })

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

-- Make Move with Jigsaw Sudoku game rules checking 
move :: Game -> (Int, Int) -> Int -> Game
move game (x,y) n | check (board game) x y n = game{board = (Board ((getNum (board game)) // [((x,y), n)]) (getBlock (board game))), message="Inserted "++ (show n) ++ " in row " ++ (show y) ++ ", col " ++ (show x) }
                | otherwise = game{message="You cannot insert "++ (show n) ++ " in row " ++ (show y) ++ ", col " ++ (show x)}

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

-- TODO: redo undo include in command
-- add parameter list in play function [((Int,Int),Int)]
