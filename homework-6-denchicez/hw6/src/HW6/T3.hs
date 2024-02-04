module HW6.T3
  ( Config (..)
  , Cell (..)
  , CellState (..)
  , Comonad19Grid
  , simulate
  ) where

import System.Random (StdGen, mkStdGen, random, randoms)
import Control.Comonad
import Data.Grid (Grid (..))
import Data.ListZipper (ListZipper (..), lLeft, lRight, lGenerator)

data Config = Config
  { probability :: Double
  , incubationPeriod :: Int
  , illnessDuration :: Int
  , immunityDuration :: Int
  , gridSize :: Int
  , iterations :: Int
  , seed :: Int
  } deriving Show

data CellState
  = Healthy
  | Infected Int
  | Ill Int
  | Immune Int
  deriving Show

data Cell = Cell
  { cellState :: CellState
  , cellRand :: StdGen
  }

instance Show Cell where
   show (Cell Healthy _) = "_"
   show (Cell (Infected _) _) = "i"
   show (Cell (Ill _) _) = "#"
   show (Cell (Immune _) _) = "@"

type Comonad19Grid = Grid Cell

rule :: Config -> Comonad19Grid -> Cell
rule config grid = do
  let cur = extract grid
  let neighbors = [pure . extract . lLeft . extract,
                   pure . extract . lRight . extract,
                   pure . extract . extract . lLeft,
                   pure . extract . extract . lRight ] >>= ($ unGrid grid)
  getNextRand True (getNextStep config cur neighbors)

isBadCell :: Cell -> Bool
isBadCell (Cell (Infected _) _) = True
isBadCell (Cell (Ill _) _) = True
isBadCell _ = False

getNextStep :: Config -> Cell -> [Cell] -> Cell
getNextStep settings (Cell Healthy randCell) neighbors = do
  let randomCurrent = 0.5 + fst (random randCell :: (Double, StdGen))
  let randBadCells = fmap
                      ((\ rand -> fst (random rand :: (Double, StdGen))) . cellRand)
                      (filter isBadCell neighbors)
  if any (\badCellRand -> probability settings ** randomCurrent >= badCellRand) randBadCells
    then getNextRand True (Cell (Infected 1) randCell)
    else getNextRand True (Cell Healthy randCell)
getNextStep settings (Cell (Infected x) rand) _ = if incubationPeriod settings <= x
                                                    then Cell (Ill 1) rand
                                                    else Cell (Infected (x + 1)) rand
getNextStep settings (Cell (Ill x) rand) _ = if illnessDuration settings <= x
                                                    then Cell (Immune 1) rand
                                                    else Cell (Ill (x + 1)) rand
getNextStep settings (Cell (Immune x) rand) _ = if immunityDuration settings <= x
                                                    then Cell Healthy rand
                                                    else Cell (Immune (x + 1)) rand

getNextRand :: Bool -> Cell -> Cell
getNextRand isLeft (Cell state gen) = do
  let rands = randoms gen :: [Int]
  let randFirst = head rands
  let randSecond = head (tail rands)
  Cell state (mkStdGen (if isLeft then randFirst else randSecond))

infectGrid :: StdGen -> Comonad19Grid -> Comonad19Grid
infectGrid gen grid = do
  let (LZ upperLines (LZ leftLines _ rightLines) lowerLines) = unGrid grid
  Grid (LZ upperLines (LZ leftLines (Cell (Infected 1) gen) rightLines) lowerLines)

-- | Creates an infinite list of grids using the given configuration.
-- Each element of this list represents one infection simulation step.
--
-- This function may take additional parameters (e.g. initial seed for random).

simulate :: Config -> [Comonad19Grid]
simulate conf = do
  let randomGenerate = mkStdGen (seed conf)
  let healthyCell = Cell Healthy randomGenerate
  let randomLine = lGenerator (getNextRand True) (getNextRand False) healthyCell
  let randomGrid = Grid (duplicate randomLine)
  let randomInfectedGrid = infectGrid randomGenerate randomGrid
  iterate (\grid -> grid =>> rule conf) randomInfectedGrid
