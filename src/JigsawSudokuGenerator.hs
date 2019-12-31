{-# LANGUAGE NamedFieldPuns, BangPatterns #-}

module JigsawSudokuGenerator where

import JigsawSudokuType
import JigsawSudokuConstant
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

-- Modified from https://hub.darcs.net/thielema/set-cover/browse/example/Sudoku.hs

-- Generate a solvable Jigsaw Sudoku Board
generateBoard :: IO Board
generateBoard = do
  result <- timeout (5 * 1000000) generate
  case result of
    Just board -> return board
    Nothing -> generateBoard
  where
    generate = generateBlocks >>= \block -> generateNum block >>= \number -> return (Board number block) 
      

-- generate blocks

generateBlocks :: IO (Array (Int, Int) Int)
generateBlocks = do
  transformBlocks regularBlocks 
  where
    transformBlocks :: (Array (Int, Int) Int) -> IO (Array (Int, Int) Int)
    transformBlocks blocks = do
      let blockPeers = map (blockCells blocks) [0..8] 
      if not (any regularBlockCheck blockPeers) 
      then do
        return blocks
      else do
        cells <- pick (filter regularBlockCheck blockPeers)
        let outer = outerCellsOfRegularBlock cells
        cell <- pick outer
        let neighbors = validNeighborCells blocks cell
        if null neighbors 
        then transformBlocks blocks
        else do
          neighbor <- pick neighbors
          let blocks' = swapCells blocks cell neighbor
          if all connectedBlockCheck (map (blockCells blocks') [blocks ! cell, blocks ! neighbor])
          then transformBlocks blocks'
          else transformBlocks blocks

-- generate number in board

generateNum :: (Array (Int, Int) Int) -> IO (Array (Int, Int) Int)
generateNum blocks = do
  a <- shuffleM $ assigns blocks
  let
    solution = head $ ESC.partitions $ ESC.bitVectorFromSetAssigns a
    !minSolution = oneSol blocks solution
    emptyBoard = listArray ((0, 0), (8, 8)) $ replicate 81 (-1)
  return $ emptyBoard // minSolution

outerCellsOfRegularBlock :: [(Int,Int)] -> [(Int,Int)]
outerCellsOfRegularBlock cells =
  filter (/= (x, y)) cells
  where
    x = 1 + (minimum $ map fst cells)
    y = 1 + (minimum $ map snd cells)

connectedBlockCheck :: [(Int,Int)] -> Bool
connectedBlockCheck cells =
  (length $ components graph) == 1
  where    
    graph :: Graph
    (graph, _, _) = graphFromEdges [(cell, cell, neighbors cells cell) | cell <- cells] 

    neighbors :: [(Int,Int)] -> (Int,Int) -> [(Int,Int)] 
    neighbors peers (x, y) = filter (\cell -> elem cell peers) [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
    
regularBlockCheck :: [(Int,Int)] -> Bool
regularBlockCheck cells =
  length rows == 3 && length columns == 3
  where
    rows = nub $ map fst cells
    columns = nub $ map snd cells

swapCells :: (Array (Int, Int) Int) -> (Int,Int) -> (Int,Int) -> (Array (Int, Int) Int)
swapCells blocks c1 c2 =
  let
    b1 = blocks ! c1
    b2 = blocks ! c2
  in
    blocks // [(c1, b2), (c2, b1)]

validNeighborCells :: (Array (Int, Int) Int) -> (Int,Int) -> [(Int,Int)]
validNeighborCells blocks (x, y) = filter (\cell -> blocks ! cell /= blocks ! (x, y)) $ filter (inRange $ bounds blocks) $ [(x-1, y-1), (x-1, y+1), (x+1, y-1), (x+1, y+1)]

blockCells :: (Array (Int, Int) Int) -> Int -> [(Int,Int)]
blockCells blocks i =
  [(x,y) | ((x,y), block) <- assocs blocks, block == i]

-- Group and sort a list based on input function

groupSortOn :: Ord b => (a -> b) -> [a] -> [[a]]
groupSortOn f xs = groupBy ((==) `on` f) $ sortOn f xs

-- Randomly pick one element from a list

pick :: [b] -> IO b
pick x = fmap (x !!) $ randomRIO (0, length x - 1)

-- make only a single solution

oneSol :: (Array (Int, Int) Int) -> [Association] -> [Association]
oneSol blocks solution = 
  reduce (ESC.initState initAssigns) [] solutionAssigns
  where
    initAssigns = ESC.bitVectorFromSetAssigns $ assigns blocks
    solutionAssigns = ESC.bitVectorFromSetAssigns $ [assign n x y b | ((x, y), n) <- solution, let b = blocks ! (x, y)]

    reduce _ xs [] = xs
    reduce state xs (y:ys) =
      case ESC.search $ foldl (flip ESC.updateState) state ys of
      [_] -> reduce state xs ys
      _ -> reduce (ESC.updateState y state) (ESC.label y : xs) ys

assign :: Int -> Int -> Int -> Int -> Assign (Set X)
assign n x y b =
   ESC.assign ((x, y), n) $
   Set.fromList [Pos x y, Row n x, Column n y, Block n b]

assigns :: (Array (Int, Int) Int) -> [Assign (Set X)]
assigns blocks = [assign n x y b | n <- [1..9], x <- [0..8], y <- [0..8], let b = blocks ! (x, y)]