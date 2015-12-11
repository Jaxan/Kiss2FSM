{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}

module Mealy where
  
{-
Originally I built upon the Data.Discrimination, providing
linear time sort/grouping. Unfortunately it is not included
in HfM. So I use GHC.Exts' groupWith, which does an n log n
sort on the key. Which is ok. The reported complexities in
this files, are hence incorrect. Comments are outdated anyways.
-}

import GHC.Exts (groupWith)
import Data.Maybe (fromJust)
import Prelude hiding (lookup)
import System.Environment (getArgs)

-- Straight forward record type
data Machine s i o = Machine
  { states :: [s]
  , inputs :: [i]
  , output :: s -> i -> o
  , delta :: s -> i -> s
  }

instance (Show s, Show i, Show o) => Show (Machine s i o) where
  show (Machine {..}) = "[Machine " ++ show states ++ " " ++ show inputs ++ " " ++ show sbeh ++ "]"
    where
      beh s i = (delta s i, output s i)
      sbeh = map (\s -> map (\i -> (s, i, beh s i)) inputs) states

-- Split states on their output. O(n) calls to output
-- and O(n) running time. Can work with any discrimination,
-- so eventually I might want :: Discriminating f => f o -> [s] -> (s -> o) -> [[s]]
-- just really is just the type of disc.
partitionOnOutput :: Ord o => Machine s i o -> [[s]]
partitionOnOutput (Machine {..}) = groupWith (\s -> map (output s) inputs) states

-- Split states on their transition, given a symbol
-- The input could be generalised to any function s -> s
-- Currently O(n^2) because of a wrong datastructure
-- Should be O(n) in future
tryRefine :: Eq s => Machine s i o -> i -> [[s]] -> [[s]]
tryRefine (Machine {..}) i partition = concat $ map (groupWith d) partition
  where
    -- TODO: define efficient data structure for this
    -- we want (amortized constant time?) State -> Int lookup
    -- where the integer is determined by the block
    -- I tried Map s Int and [(Set s, Int)], both were much slower :(
    d s = lookup (delta s i) indexedPartition
    indexedPartition = zip partition [0 :: Int ..]
    lookup x [] = undefined
    lookup x ((ss,y):ys) = if elem x ss then y else lookup x ys

-- Refine with all inputs once
refine :: Eq s => Machine s i o -> [[s]] -> [[s]]
refine m partition = foldl1 (.) (map (tryRefine m) (inputs m)) partition

-- Refine until stable. In this case we stop depending on
-- the size of the machine. Ultimately we want to do this
-- by comparing the partitions (just by counting)
-- n*p calls to tryRefine, hence (in the future) O(pn^2)
moore :: (Eq s, Ord o) => Machine s i o -> [[s]]
moore m = foldl1 (.) (replicate n r) acceptablePartition
  where
    r = refine m
    n = length (states m)
    acceptablePartition = partitionOnOutput m

-- Moore with early break
mooreF :: (Eq s, Ord o) => Machine s i o -> [[s]]
mooreF m = go acceptablePartition
  where
    acceptablePartition = partitionOnOutput m
    go p
      | length (refine m p) == length p = p
      | otherwise = go (refine m p)
