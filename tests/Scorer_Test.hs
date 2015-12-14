module Scorer_Test
  (tests)
where

import Test.Tasty
--import Test.Tasty.SmallCheck as SC
--import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Ord

tests :: TestTree
tests = testGroup "Scorer tests" [unitTests]

unitTests = testGroup "Unit tests"
  []

