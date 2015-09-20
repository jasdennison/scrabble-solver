module Trie
  (Trie,
   emptyTrie,
   insertTrie,
   findTrie)
where

import qualified Data.Map as Map
import Control.DeepSeq

data Trie a = Trie (Map.Map a (Bool, Trie a))

instance NFData a => NFData (Trie a) where
  rnf (Trie x) = rnf x

insertTrie :: Ord a => Trie a -> [a] -> Trie a
insertTrie (Trie t) [x] = case Map.lookup x t of
                            Just (True, t') -> Trie t
                            Just (False, t') -> Trie (Map.update (\x -> Just (True, t')) x t)
                            Nothing -> Trie (Map.insert x (True, emptyTrie) t)
insertTrie (Trie t) (x:xs) = case Map.lookup x t of
                               Just (b, t') -> Trie (Map.update (\x -> Just (b, insertTrie t' xs)) x t)
                               Nothing -> Trie (Map.insert x (False, insertTrie emptyTrie xs) t)

emptyTrie :: Ord a => Trie a
emptyTrie = Trie Map.empty

findTrie :: Ord a => Trie a -> [a] -> Bool
findTrie (Trie t) [x] = case Map.lookup x t of
                          Just (b, t') -> b
                          Nothing -> False
findTrie (Trie t) (x:xs) = case Map.lookup x t of
                             Just (b, t') -> findTrie t' xs
                             Nothing -> False

