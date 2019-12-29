module JigsawSudokuType where
   
import Graphics.Gloss
import Data.Array
import Graphics.Gloss.Data.Color

-- First matrix records number in the Board, -1 as not filled
-- The second matrix records the board location
data Board = Board (Array (Int, Int) Int) (Array (Int, Int) Int) deriving (Eq)

data Game = Game {board :: Board, message :: String, blockColors :: [Color], originalBoard :: Board} deriving (Eq)

-- TODO: add game state 'win/in progress'
data GameState = GameState {game :: Game, currentCell :: (Int, Int), solution :: Board}


