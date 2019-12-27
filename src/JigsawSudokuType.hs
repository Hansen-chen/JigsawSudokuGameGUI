module JigsawSudokuType where
   
import Data.Map as Map
import Data.Char
import Data.List
import Graphics.Gloss


type Location = (Int, Int)

data CellCoordinates = CellCoordinates
  { cellCenter :: Point
  , cellTopLeft :: Point
  , cellTopRight :: Point
  , cellBottomLeft :: Point
  , cellBottomRight :: Point
  }

data BoundaryType = WorldBoundary | Wall | AdjacentCell Location
  deriving (Show, Eq)
data CellBoundaries = CellBoundaries
  { upBoundary :: BoundaryType
  , rightBoundary :: BoundaryType
  , downBoundary :: BoundaryType
  , leftBoundary :: BoundaryType
  }
  deriving (Show, Eq)

type DirectionLens = CellBoundaries -> BoundaryType

data World = World
  { playerLocation :: Location
  , startLocation :: Location
  , endLocation :: Location
  , worldBoundaries :: Map.Map Location CellBoundaries
  }

