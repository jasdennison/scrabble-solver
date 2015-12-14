module Parsers_Test
  (tests)
where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Array.IArray
import Data.List
import qualified Data.Map.Strict as Map
import Data.Ord
import Text.Parsec hiding (Empty)

import Board hiding (Direction(..))
import qualified Parsers as P
import Scorer

tests :: TestTree
tests = testGroup "Parser tests" [unitTests]

unitTests = testGroup "Unit tests"
  [ testCase "Parse valid tile hand" $
      parseTileHand "abcd fg" @?= Right [ ValueTile 'a', ValueTile 'b'
                                        , ValueTile 'c' , ValueTile 'd'
                                        , BlankTile, ValueTile 'f'
                                        , ValueTile 'g'
                                        ]

  , testCase "Parse invalid tile hand" $
      case parseTileHand "abcdefgh" of
        Left e -> assertString ""
        Right th -> assertFailure "Parsed invalid tile hand."

  , testCase "Parse valid board design" $
      case parseBoardDesign validBoardDesign of
        Left e -> assertFailure "Failed to parse valid board design"
        Right bd -> do
          assertBool "Parsing dimension" $
            bdDimension bd == (4, 4)
          assertBool "Parsing letter scores" $
            Map.lookup 'a' (bdLetterScores bd) == Just 1 &&
            Map.lookup 'b' (bdLetterScores bd) == Just 2 &&
            Map.lookup 'c' (bdLetterScores bd) == Just 15
          assertBool "Parsing bonus" $
            bdBonus bd == 10
          assertBool "Parsing multipliers" $
            bdMultipliers bd ! (1, 1) == DW &&
            bdMultipliers bd ! (1, 2) == TW &&
            bdMultipliers bd ! (2, 1) == DL &&
            bdMultipliers bd ! (2, 2) == TL &&
            bdMultipliers bd ! (4, 4) == None

  , testCase "Parse valid board" $
      case parseBoard validBoard of
        Left e -> assertFailure "Failed to parse valid board"
        Right b -> do
          assertBool "Parsing dimension" $
            bDimension b == (4, 4)
          assertBool "Parsing tiles" $
            bPlaces b ! (1, 1) == Full (MarkedT 'a') &&
            bPlaces b ! (1, 2) == Full (BlankT 'b') &&
            bPlaces b ! (4, 4) == Empty

  ]

parseTileHand = parse P.parseTileHand "test"
parseBoardDesign = parse P.parseBoardDesign "test"
parseBoard = parse (P.parseBoard (4, 4)) "test"

validBoardDesign :: String
validBoardDesign = unlines
  [ "4 4"
  , "a=1 b=2 c=15"
  , "10"
  , "DT.."
  , "23.."
  , "...."
  , "...."
  ]

validBoard :: String
validBoard = unlines
  [ "aB.."
  , "...."
  , "...."
  , "...."
  ]

