{-# LANGUAGE NamedFieldPuns, BangPatterns #-}

module JigsawSudokuGenerator where

import JigsawSudokuType
import System.Random (randomRIO)
import System.Random.Shuffle
import System.Timeout (timeout)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List
import Data.Function
import Data.Array
import Data.Graph
import qualified Math.SetCover.Exact as ESC
import Control.Monad

-- TODO: change code variable?

-- | Sort and group a list based on a function
groupSortOn :: Ord b => (a -> b) -> [a] -> [[a]]
groupSortOn f xs = groupBy ((==) `on` f) $ sortOn f xs

-- | Randomly choose an element from a list
pick :: [a] -> IO a
pick xs = fmap (xs !!) $ randomRIO (0, length xs - 1)

generateBoard :: IO Board
generateBoard = do
  result <- timeout (5 * 1000000) generate
  case result of
    Just board -> return board
    Nothing -> generateBoard
  where
    generate = do
      block <- generateBlocks
      number <- generateNum block
      return (Board number block)

-- generate blocks

generateBlocks :: IO (Array (Int, Int) Int)
generateBlocks = do
  -- start with regular blocks layout
  transformBlocks regularBlocks 
  where
    transformBlocks :: (Array (Int, Int) Int) -> IO (Array (Int, Int) Int)
    transformBlocks blocks = do
      let blockPeers = map (blockCells blocks) [0..8] 
      -- repeat until no regular blocks
      if not $ any isRegularBlock blockPeers 
      then do
        return blocks
      else do
        --  pick an outer cell from a random regular block
        cells <- pick $ filter isRegularBlock blockPeers
        let outer = outerCellsOfRegularBlock cells
        cell <- pick outer
        let neighbors = validNeighbors blocks cell

        -- retry if no valid neighbors
        if null neighbors 
        then transformBlocks blocks
        else do
          neighbor <- pick neighbors
          --  swap an outer cell with a neighbor cell of another block
          let blocks' = swapCells blocks cell neighbor

          --  retry if the blocks become disconnected
          if all isConnectedBlock $ map (blockCells blocks') [blocks ! cell, blocks ! neighbor]
          then transformBlocks blocks'
          else transformBlocks blocks

regularBlocks :: (Array (Int, Int) Int)
regularBlocks = 
  array ((0, 0), (8, 8)) $ 
  [((r, c), (r `div` 3) * 3 + (c `div` 3)) | r <- [0..8], c <- [0..8]]

swapCells :: (Array (Int, Int) Int) -> (Int,Int) -> (Int,Int) -> (Array (Int, Int) Int)
swapCells blocks c1 c2 =
  let
    b1 = blocks ! c1
    b2 = blocks ! c2
  in
    blocks // [(c1, b2), (c2, b1)]

validNeighbors :: (Array (Int, Int) Int) -> (Int,Int) -> [(Int,Int)]
validNeighbors blocks (r, c) =
  filter (\cell -> blocks ! cell /= blocks ! (r, c)) $
  filter (inRange $ bounds blocks) $
  [(r-1, c-1), (r-1, c+1), (r+1, c-1), (r+1, c+1)]

blockCells :: (Array (Int, Int) Int) -> Int -> [(Int,Int)]
blockCells blocks i =
  [(x,y) | ((x,y), block) <- assocs blocks, block == i]

outerCellsOfRegularBlock :: [(Int,Int)] -> [(Int,Int)]
outerCellsOfRegularBlock cells =
  filter (/= (r, c)) cells
  where
    r = 1 + (minimum $ map fst cells)
    c = 1 + (minimum $ map snd cells)

isConnectedBlock :: [(Int,Int)] -> Bool
isConnectedBlock cells =
  (length $ components graph) == 1
  where
    neighbors :: [(Int,Int)] -> (Int,Int) -> [(Int,Int)] 
    neighbors peers (r, c) = 
      filter (\cell -> elem cell peers) [(r-1, c), (r+1, c), (r, c-1), (r, c+1)]
    
    graph :: Graph
    (graph, _, _) = graphFromEdges [(cell, cell, neighbors cells cell) | cell <- cells] 
    
isRegularBlock :: [(Int,Int)] -> Bool
isRegularBlock cells =
  length rows == 3 && length columns == 3
  where
    rows = nub $ map fst cells
    columns = nub $ map snd cells

-- generate board
-- Modified from https://hub.darcs.net/thielema/set-cover/browse/example/Sudoku.hs

generateNum :: (Array (Int, Int) Int) -> IO (Array (Int, Int) Int)
generateNum blocks = do
  a <- shuffleM $ assigns blocks
  let
    solution = head $ ESC.partitions $ ESC.bitVectorFromSetAssigns a
    !minSolution = minimizeSolution blocks solution
    emptyBoard = listArray ((0, 0), (8, 8)) $ replicate 81 (-1)
  return $ emptyBoard // minSolution

-- minimize: a single solution

assign :: Int -> Int -> Int -> Int -> Assign (Set X)
assign n r c b =
   ESC.assign ((r, c), n) $
   Set.fromList [Pos r c, Row n r, Column n c, Block n b]

assigns :: (Array (Int, Int) Int) -> [Assign (Set X)]
assigns blocks = [assign n r c b | n <- [1..9], r <- [0..8], c <- [0..8], let b = blocks ! (r, c)]

minimizeSolution :: (Array (Int, Int) Int) -> [Association] -> [Association]
minimizeSolution blocks solution = 
  reduce (ESC.initState initAssigns) [] solutionAssigns
  where
    initAssigns = ESC.bitVectorFromSetAssigns $ assigns blocks
    solutionAssigns = ESC.bitVectorFromSetAssigns $ [assign n r c b | ((r, c), n) <- solution, let b = blocks ! (r, c)]

    reduce _ xs [] = xs
    reduce state xs (y:ys) =
      case ESC.search $ foldl (flip ESC.updateState) state ys of
      [_] -> reduce state xs ys
      _ -> reduce (ESC.updateState y state) (ESC.label y : xs) ys
