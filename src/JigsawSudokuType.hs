module JigsawSudokuType where
   
import Graphics.Gloss
import Data.Array
import Graphics.Gloss.Data.Color
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Math.SetCover.Exact as ESC



-- First matrix records number in the Board, -1 as not filled
-- The second matrix records the board location

data Board = Board (Array (Int, Int) Int) (Array (Int, Int) Int) deriving (Eq)

-- Represent a jigsaw sudoku game

data Game = Game {board :: Board, message :: String, blockColors :: [Color], originalBoard :: Board, filename :: String} deriving (Eq)

-- Represent a jigsaw sudoku game playing in GUI

data GameState = GameState {game :: Game, currentCell :: (Int, Int), solution :: Board, initialBoard :: Board,moves :: [((Int,Int), Int)], gamePointer :: Int} deriving (Eq)

-- Modified from https://hub.darcs.net/thielema/set-cover/browse/example/Sudoku.hs
-- Transtive data types for solver

data X = Pos Int Int | Row Int Int | Column Int Int | Block Int Int
         deriving (Eq, Ord, Show)

type Association = ((Int, Int), Int)

type Assign = ESC.Assign Association


