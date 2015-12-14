module Trie_Test
  (tests)
where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Ord
import Trie

tests :: TestTree
tests = testGroup "Trie tests" [ unitTests
                               , qcProps
                               ]

unitTests = testGroup "Unit tests"
  [ testCase "Cannot insert empty into a Trie" $
      findTrie (insertTrie emptyTrie []) ([] :: String) @?= False
  ]

qcProps = testGroup "Trie properties"
  [ QC.testProperty "Find a non-empty string inserted into a Trie" $
      forAll (listOf1 arbitrary) $ \cs -> findTrie (insertTrie emptyTrie cs) (cs :: String)

  , QC.testProperty "Cannot find prefix of string inserted into a Trie" $
      forAll (listOf1 arbitrary) $ \cs ->
      forAll (choose (0, length cs - 1)) $ \n ->
      not (findTrie (insertTrie emptyTrie cs) (take (n :: Int) (cs :: String)))
  ]

