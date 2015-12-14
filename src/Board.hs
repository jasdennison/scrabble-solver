{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Board
  (Dimension,
   Position,
   WordPlacementMap,
   Direction(..),
   BoardCell(..),
   TileValue(..),
   Board(..),
   TilePlacement(..),
   WordPlacement(..),
   tileToChar,
   joinPoints,
   genWordPlacements,
   genTilePlacement,
   buildWordPlacementMap,
   rmdups)
where

import Prelude hiding (Left, Right)
import Control.DeepSeq
import Data.Array.IArray
import Data.List
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)

type Dimension = (Int, Int)
type Position = (Int, Int)
type WordPlacementMap = Map.Map Int [WordPlacement]

data Direction = Up | Down | Left | Right
  deriving (Show, Eq, Ord, Generic, NFData)

data BoardCell = Empty | Full TileValue
  deriving (Show, Eq)

data TileValue = BlankT Char | MarkedT Char
  deriving (Show, Eq, Ord, Generic, NFData)

data Board = Board {
  bDimension :: Dimension,
  bPlaces :: Array Position BoardCell,
  bTiles :: Array Position TilePlacement
  }

data TilePlacement = TileP {
  tPosition :: Position,
  tUp :: [TileValue],
  tUpS :: String,
  tDown :: [TileValue],
  tDownS :: String,
  tLeft :: [TileValue],
  tLeftS :: String,
  tRight :: [TileValue],
  tRightS :: String
  } deriving (Show)

data WordPlacement = WordP {
  wDirection :: Direction,
  wPosition :: Position,
  wTiles :: [TilePlacement]
  } deriving (Show)


genWordPlacements :: Board -> Position -> [WordPlacement]
genWordPlacements b (i, j) = acrossWords ++ downWords
  where acrossWords = genWordPlacements' b (i, j) Right
        downWords = genWordPlacements' b (i, j) Down

genWordPlacements' :: Board -> Position -> Direction -> [WordPlacement]
genWordPlacements' b (i, j) dir = concatMap (genOffsetWordPlacements b dir) positionOffsets
  where positionOffsets = offsets b ((i, j), 0) dir

genOffsetWordPlacements :: Board -> Direction -> (Position, Int) -> [WordPlacement]
genOffsetWordPlacements b dir ((i, j), offset) = fmap mkWord offsetTilePlacements
  where tilePlacements = tail (inits (genTilePlacements b dir (i, j) 7))
        offsetTilePlacements = filter ((>offset) . length) tilePlacements
        mkWord ts = WordP { wDirection = dir, wPosition = (i, j), wTiles = ts }

genTilePlacements :: Board -> Direction -> Position -> Int -> [TilePlacement]
genTilePlacements b dir (i, j) 0 = []
genTilePlacements b dir (i, j) n
  | onBoard b (i, j) =
      if empty b (i, j)
         then bTiles b ! (i, j) : genTilePlacements b dir pos (n-1)
         else genTilePlacements b dir pos n
  | otherwise = []
  where pos = nextPos (i, j) dir

genTilePlacement :: Board -> Position -> TilePlacement
genTilePlacement b (i, j) =
  TileP { tPosition = (i, j)
        , tUp = up
        , tUpS = fmap tileToChar up
        , tDown = down
        , tDownS = fmap tileToChar down
        , tLeft = left
        , tLeftS = fmap tileToChar left
        , tRight = right
        , tRightS = fmap tileToChar right }
  where up = reverse (connectedString b (i, j) Up)
        down = connectedString b (i, j) Down
        left = reverse (connectedString b (i, j) Left)
        right = connectedString b (i, j) Right

connectedString :: Board -> Position -> Direction -> [TileValue]
connectedString b (i, j) dir =
  if onBoard b pos
     then
       case bPlaces b ! pos of
         Empty -> []
         Full (MarkedT c) -> MarkedT c : connectedString b pos dir
         Full (BlankT c) -> BlankT c : connectedString b pos dir
     else []
  where pos = nextPos (i, j) dir

tileToChar :: TileValue -> Char
tileToChar (MarkedT c) = c
tileToChar (BlankT c) = c

offsets :: Board -> (Position, Int) -> Direction -> [(Position, Int)]
offsets b ((i, j), n) dir
  | n == 0 = ((i, j), 0) : offsets b (pos, 1) dir
  | otherwise = if onBoard b (i, j) && empty b (i, j) && not (connected b (i, j)) && n < 7
                   then ((i, j), n) : offsets b (pos, n+1) dir
                   else []
  where pos = nextPos (i, j) (oppDirection dir)

connected :: Board -> Position -> Bool
connected b (i, j) = or [onBoard b p && not (empty b p) | p<-adjacents (i, j)]

onBoard :: Board -> Position -> Bool
onBoard b (i, j) = i >= 1 && i <= h && j >= 1 && j <= w
  where (h, w) = bDimension b

empty :: Board -> Position -> Bool
empty b (i, j) =
  case bPlaces b ! (i, j) of
    Empty -> True
    _ -> False

nextPos :: Position -> Direction -> Position
nextPos (i, j) Up = (i-1, j)
nextPos (i, j) Down = (i+1, j)
nextPos (i, j) Left = (i, j-1)
nextPos (i, j) Right = (i, j+1)

oppDirection :: Direction -> Direction
oppDirection Up = Down
oppDirection Down = Up
oppDirection Left = Right
oppDirection Right = Left

joinPoints :: Board -> [Position]
joinPoints b =
  case emptyAdjacents of
    [] -> [midPoint (h, w)]
    _ -> rmdups emptyAdjacents
  where (h, w) = bDimension b
        tilePositions = [(i, j) | i<-[1..h], j<-[1..w], not (empty b (i, j))]
        emptyAdjacents = filter validEmpty (concatMap adjacents tilePositions)
        validEmpty p = onBoard b p && empty b p

midPoint :: Dimension -> (Int, Int)
midPoint (h, w) = (hq + hr, wq + wr)
  where (hq, hr) = h `divMod` 2
        (wq, wr) = w `divMod` 2

adjacents :: Position -> [Position]
adjacents (i, j) = [(i, j-1), (i, j+1), (i-1, j), (i+1, j)]

buildWordPlacementMap :: [WordPlacement] -> WordPlacementMap
buildWordPlacementMap ws = foldr insert' Map.empty ws
  where insert' w m = Map.insertWith (flip (++)) (length . wTiles $ w) [w] m

rmdups :: Ord a => [a] -> [a]
rmdups = fmap head . group . sort

