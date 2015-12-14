module Parsers
  (parseBoard,
   parseBoardDesign,
   parseTileHand)
where

import Control.Applicative hiding ((<|>))
import Control.Monad
import Data.Array.IArray
import Data.Char
import qualified Data.Map.Strict as Map
import Text.Parsec hiding (Empty)
import Text.Parsec.String
import Text.Parsec.Extra (eol)
import Board hiding (Left, Right)
import Scorer

boardCell :: Parser BoardCell
boardCell = Empty <$ char '.'
        <|> Full . MarkedT <$> lower
        <|> Full . BlankT . toLower <$> upper
        <?> "board character"

row :: Int -> Parser a -> Parser [a]
row w p = do
  cs <- count w p
  eol
  return cs

parseBoard :: Dimension -> Parser Board
parseBoard (h, w) = do
  boardCells <- count h (row w boardCell)
  let elementsArray = array ((1, 1), (h, w)) (zip indices (concat boardCells))
  let emptyTilesArray = array ((1,1),(0,0)) []
  let board = Board { bDimension = (h, w), bPlaces = elementsArray, bTiles = emptyTilesArray }
  let tiles = fmap (genTilePlacement board) indices
  let tilesArray = array ((1, 1), (h, w)) (zip indices tiles)
  return $ board { bTiles = tilesArray }
    where indices = [(i, j) | i<-[1..h], j<-[1..w]]

multiplierCell :: Parser Multiplier
multiplierCell = None <$ char '.'
             <|> DL <$ char '2'
             <|> TL <$ char '3'
             <|> DW <$ char 'D'
             <|> TW <$ char 'T'
             <?> "multiplier character"

dimension :: Parser Dimension
dimension = do
  h <- many1 digit
  spaces
  w <- many1 digit
  eol
  return (read h :: Int, read w :: Int)

bonus :: Parser Int
bonus = do
  b <- many1 digit
  eol
  return (read b :: Int)

letterScore :: Parser (Char, Int)
letterScore = do
  l <- lower
  char '='
  score <- many1 digit
  return (l, read score :: Int)

letterScores :: Parser LetterScoresMap
letterScores = do
  scores <- letterScore `sepBy` char ' '
  eol
  return $ foldr (uncurry Map.insert) Map.empty scores

parseBoardDesign :: Parser BoardDesign
parseBoardDesign = do
  (h, w) <- dimension
  ls <- letterScores
  b <- bonus
  multiplierCells <- count h (row w multiplierCell)
  let indices = [(i, j) | i<-[1..h], j<-[1..w]]
  let multipliers = array ((1, 1), (h, w)) (zip indices (concat multiplierCells))
  return BoardDesign { bdDimension=(h, w), bdLetterScores=ls, bdBonus=b, bdMultipliers=multipliers }

tile :: Parser Tile
tile = ValueTile <$> lower
   <|> BlankTile <$ char ' '
   <?> "tile character"

parseTileHand :: Parser [Tile]
parseTileHand = do
  hand <- upTo 7 tile
  eof
  return hand

upTo :: Int -> Parser a -> Parser [a]
upTo n p
   | n <= 0 = return []
   | otherwise = option [] $ liftM2 (:) p (upTo (n-1) p)

