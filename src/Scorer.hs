{-# LANGUAGE DeriveAnyClass, DeriveGeneric, NamedFieldPuns #-}

module Scorer
  (Dictionary,
   LetterScoresMap,
   Tile(..),
   Multiplier(..),
   BoardDesign(..),
   Move(..),
   genTilePermutations,
   applyWordPlacements)
where

import Prelude hiding (Left, Right)
import Control.DeepSeq
import Data.Array.IArray
import Data.Char
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import GHC.Generics (Generic)
import Board
import Trie

type Dictionary = Trie Char
type LetterScoresMap = Map.Map Char Int

data Tile = BlankTile | ValueTile Char
  deriving (Eq, Show)

data Multiplier = None | DL | TL | DW | TW
  deriving (Show)

data BoardDesign = BoardDesign {
  bdDimension :: Dimension,
  bdLetterScores :: LetterScoresMap,
  bdBonus :: Int,
  bdMultipliers :: Array Position Multiplier
  }

data Move = Move {
  mDirection :: Direction,
  mPosition :: Position,
  mWord :: String,
  mScore :: Int,
  mTiles :: [TileValue]
  } deriving (Eq, Ord, Generic, NFData)


applyWordPlacements :: BoardDesign -> Dictionary -> WordPlacementMap -> [TileValue] -> [Move]
applyWordPlacements bd dict wpMap perm = case Map.lookup (length perm) wpMap of
                                           Just ws -> mapMaybe (applyWordPlacement bd dict perm) ws
                                           Nothing -> []

applyWordPlacement :: BoardDesign -> Dictionary -> [TileValue] -> WordPlacement -> Maybe Move
applyWordPlacement bd dict perm (WordP { wDirection, wPosition, wTiles }) =
  if validWord && validCrossWords
    then Just Move { mDirection = wDirection, mPosition = wPosition, mWord = word, mScore = score, mTiles = perm }
    else Nothing
  where word = constructWord wDirection wTiles perm
        validWord = validateWord dict word
        crossWords = zipWith (constructCrossWord wDirection) wTiles perm
        validCrossWords = all (validateWord dict) crossWords
        wordScore = scoreMainWord bd wDirection wTiles perm
        crossWordsScore = sum (zipWith (scoreCrossWord bd wDirection) wTiles perm)
        score = wordScore + crossWordsScore

constructWord :: Direction -> [TilePlacement] -> [TileValue] -> String
constructWord dir [] tiles = []
constructWord dir (t:ts) tiles = wordPrefix ++ concat (zipWith (assignTile dir) (t:ts) tiles)
  where wordPrefix = tilesToString (prefix dir t)

tilesToString :: [TileValue] -> String
tilesToString [] = []
tilesToString (MarkedT c : ts) = c : tilesToString ts
tilesToString (BlankT c : ts) = c : tilesToString ts

prefix :: Direction -> TilePlacement -> [TileValue]
prefix Right t = tLeft t
prefix Down t = tUp t

assignTile :: Direction -> TilePlacement -> TileValue -> String
assignTile Right t tile = tilesToString (tile : tRight t)
assignTile Down t tile = tilesToString (tile : tDown t)

constructCrossWord :: Direction -> TilePlacement -> TileValue -> String
constructCrossWord Right t tile = tilesToString (tUp t) ++ tilesToString (tile : tDown t)
constructCrossWord Down t tile = tilesToString (tLeft t) ++ tilesToString (tile : tRight t)

scoreMainWord :: BoardDesign -> Direction -> [TilePlacement] -> [TileValue] -> Int
scoreMainWord bd Right [TileP { tLeft = [], tRight = [] }] [tile] = 0 
scoreMainWord bd Down [TileP { tUp = [], tDown = [] }] [tile] = 0 
scoreMainWord bd dir ts tiles = applyMultipliers wordMultipliers score + bonus
  where (tileScore, wordMultipliers) = foldr (scoreMainWord' bd) (0, []) (zip ts tiles)
        prefixScore = scoreTiles bd (prefix dir (head ts))
        interimScores = sum (fmap (scoreInterim bd dir) ts)
        score = prefixScore + tileScore + interimScores
        bonus = if length ts == 7 then bdBonus bd else 0

scoreMainWord' :: BoardDesign -> (TilePlacement, TileValue) -> (Int, [Multiplier]) -> (Int, [Multiplier])
scoreMainWord' bd (t, tile) (score, ms) = case bdMultipliers bd ! tPosition t of
                                            None -> (score + tileScore, ms)
                                            DL -> (score + (2*tileScore), ms)
                                            TL -> (score + (3*tileScore), ms)
                                            DW -> (score + tileScore, DW:ms)
                                            TW -> (score + tileScore, TW:ms)
  where tileScore = scoreTile bd tile

scoreTile :: BoardDesign -> TileValue -> Int
scoreTile bd (BlankT c) = 0
scoreTile bd (MarkedT c) = fromMaybe 0 (Map.lookup c (bdLetterScores bd))

scoreTiles :: BoardDesign -> [TileValue] -> Int
scoreTiles bd tiles = sum (fmap (scoreTile bd) tiles)

applyMultipliers :: [Multiplier] -> Int -> Int
applyMultipliers [] n = n
applyMultipliers (m:ms) n = case m of
                              DW -> applyMultipliers ms (2*n)
                              TW -> applyMultipliers ms (3*n)
                              _ -> applyMultipliers ms n

scoreInterim :: BoardDesign -> Direction -> TilePlacement -> Int
scoreInterim bd Right t = scoreTiles bd (tRight t)
scoreInterim bd Down t = scoreTiles bd (tDown t)

scoreCrossWord :: BoardDesign -> Direction -> TilePlacement -> TileValue -> Int
scoreCrossWord bd Right (TileP { tUp = [], tDown = [] }) tile = 0
scoreCrossWord bd Right (TileP { tPosition, tUp, tDown }) tile = scoreCrossWord' bd tPosition (tUp, tile, tDown)
scoreCrossWord bd Down (TileP { tLeft = [], tRight = [] }) tile = 0
scoreCrossWord bd Down (TileP { tPosition, tLeft, tRight }) tile = scoreCrossWord' bd tPosition (tLeft, tile, tRight)

scoreCrossWord' :: BoardDesign -> Position -> ([TileValue], TileValue, [TileValue]) -> Int
scoreCrossWord' bd p (prefix, tile, suffix) = case bdMultipliers bd ! p of
                                                None -> wordScore
                                                DL -> prefixScore + (2 * tileScore) + suffixScore
                                                TL -> prefixScore + (3 * tileScore) + suffixScore
                                                DW -> 2 * wordScore
                                                TW -> 3 * wordScore
  where prefixScore = scoreTiles bd prefix
        suffixScore = scoreTiles bd suffix
        tileScore = scoreTile bd tile
        wordScore = prefixScore + tileScore + suffixScore

validateWord :: Dictionary -> String -> Bool
validateWord dict w = (length w == 1) || findTrie dict w

genTilePermutations :: [Tile] -> [[TileValue]] 
genTilePermutations tiles = concatMap permutations (concatMap subsequences ts)
  where ts = expandBlanks tiles

expandBlanks :: [Tile] -> [[TileValue]]
expandBlanks [] = [[]]
expandBlanks (t:ts) = case t of
                        BlankTile -> concat [fmap (BlankT c :) rest | c<-['a'..'z']]
                        ValueTile c -> fmap (MarkedT c :) rest
  where rest = expandBlanks ts

