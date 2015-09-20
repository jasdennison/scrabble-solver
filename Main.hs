{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.DeepSeq
import Control.Exception
import Control.Parallel.Strategies
import Data.Aeson hiding (json)
import Data.Char
import Data.Function
import Data.List
import Data.Text (pack)
import Data.Typeable
import Network.HTTP.Types.Status
import Text.Parsec hiding (Empty)
import Web.Spock.Safe hiding (head)
import Board hiding (Left, Right)
import Parsers
import Scorer
import Trie

dictionaryPath = "config/enable1.dict"

instance ToJSON Move where
  toJSON m = object ["word" .= mWord m,
                     "direction" .= show (mDirection m),
                     "position" .= mPosition m,
                     "score" .= mScore m,
                     "tiles" .= printTiles (mTiles m)]

printTiles :: [TileValue] -> String
printTiles [] = []
printTiles (MarkedT c : ts) = c : printTiles ts
printTiles (BlankT c : ts) = toUpper c : printTiles ts

findMoves :: BoardDesign -> Board -> Dictionary -> [Tile] -> [Move]
findMoves boardDesign board dictionary tiles = filter ((>1) . length . mWord) (concat moves)
  where tilePerms = genTilePermutations tiles
        wordPlacements = concatMap (genWordPlacements board) (joinPoints board)
        wordPlacementMap = buildWordPlacementMap wordPlacements
        moves = parallelMap (applyWordPlacements boardDesign dictionary wordPlacementMap) tilePerms

parallelMap :: NFData b => (a -> b) -> [a] -> [b]
parallelMap f xs = fmap f xs `using` parListChunk 1000 rdeepseq

solveBoard :: String -> String -> String -> Dictionary -> Either String [Move]
solveBoard boardDesign board tileHand dictionary = do 
  ts <- parse parseTileHand "stdin" tileHand
    `failWith` "Tile hand failed to parse"

  bd <- parse parseBoardDesign "stdin" boardDesign
    `failWith` "Board Design failed to parse"

  b <- parse (parseBoard (bdDimension bd)) "stdin" board
    `failWith` "Board failed to parse"

  let moves = findMoves bd b dictionary ts
  return $ (sortBy (flip compare `on` mScore) (rmdups moves))

instance Exception ParseError
deriving instance Typeable ParseError

failWith ::  Exception a => Either a b -> String -> Either String b
failWith either msg  = case either of
                         Left e -> Left msg
                         Right v -> return v

main :: IO ()
main = do
  dictionaryFile <- readFile dictionaryPath
  let dictionaryWords = filter (isLower . head) (lines dictionaryFile)
  let dictionary = force $ foldl' insertTrie emptyTrie dictionaryWords
 
  runSpock 8080 $ spockT id $
    do get root $
         text "Scrabble Solver."
       --get ("solve" <//> var <//> var) $ solveR
       get "solve" $ (solveR dictionary)
                     
solveR :: Dictionary -> ActionT IO ()
solveR dictionary = do
  boardDesign <- param' "boardDesign"
  board <- param' "board"
  tileHand <- param' "tileHand"
  case solveBoard boardDesign board tileHand dictionary of
    Left e -> do setStatus notAcceptable406
                 text $ pack e
    Right a -> do setHeader "Content-Type" "application/json; charset=utf-8"
                  lazyBytes $ encode a

