 {-# LANGUAGE BangPatterns #-}

-------------------------------------------------------------------------
--                              
--         Frequency.hs
--                              
--         Calculate the frequencies of words in a text, used in
--         Huffman coding.
-------------------------------------------------------------------------
module Frequency
  ( frequency
  ) where

import Prelude hiding (lookup)
import Data.Map (Map, lookup, insert, empty, assocs, unionWith)

-- A frequency histogram is a map from values to `Int`s
type Histogram a = Map a Int


-- | inc
-- Increment the value associated with the given key in the input histogram.
-- WARNING: Do not change this function!
-- Example:
-- >>> inc empty 'a'
-- fromList [('a',1)]
inc :: Ord a => Histogram a -> a -> Histogram a
inc !h x =
  case lookup x h of
    Nothing -> insert x 1 h
    Just !n -> insert x (n + 1) h

-- Calculate the frequencies of characters in a list.
frequency :: Ord a => [a] -> [(a, Int)]
frequency xs = assocs (create empty xs)

-- | create
-- Create a new histogram that augments the input histogram with
-- the keys given in the input list.
--
-- Example:
-- >>> create empty "abacab"
-- fromList [('a',3),('b',2),('c',1)]
create :: Ord a => Histogram a -> [a] -> Histogram a
create _ [] = empty
create h (x:xs) = unionWith (+) (inc h x)(create h xs)




 