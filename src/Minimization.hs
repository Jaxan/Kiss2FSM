{-# LANGUAGE ViewPatterns #-}

module Minimization where

{-
 This is an implementation of Moore's algorithm for minimization. It is
 general enough to handle deterministic complete DFAs and Mealy machines,
 and probably some other types as well. The current data structure is not
 the most efficient one, I guess. I believe the complexity is approx.
 O(n^2 log n), which can be O(n^2). But performance for my purpose was good
 enough. (I.e. it can handle a Mealy machine with 3410 states and 78 inputs
 within 20 seconds.)
-}

import           Control.Arrow   ((&&&))
import           Data.Map.Strict (Map, empty, insert, lookup)
import           Data.Maybe      (fromJust)
import           GHC.Exts        (groupWith)

import           Prelude         hiding (lookup)

-- Data structure is still a bit naive, but good enough
type Class = Int
type Partition s = (Map s Class, [[s]])

privTo :: Ord s => [[s]] -> Partition s
privTo p = (go 0 p, p)
  where
    go _ [] = empty
    go n (x:xs) = foldr (\s -> insert s n) (go (n+1) xs) x
privFrom :: Partition s -> [[s]]
privFrom = snd

-- "color" / class of a state in the partition
-- It is only well defined if the element occurs in the partition
color :: Ord s => Partition s -> s -> Class
color (m, _) x = fromJust $ lookup x m

-- The kind-of inverse of color, picks a representive from a class
representative :: Partition s -> Class -> s
representative p i = head $ snd p !! i

-- Number of classes
size :: Partition s -> Int
size p = length (snd p)

-- Here is the magic. Simple partition refinement. Not really efficient.
-- But good enough in this package. The Ord constraint is used to search
-- for a state in our data structre. The Ord constraint is used to
-- discriminate outputs. Ideally, we would use the discrimination package
-- for both of these things.
minimize :: (Ord s, Ord o) => [s] -> (s -> o) -> [s -> s] -> Partition s
minimize states output relations = go $ initialPartition output states
  where go (refine relations &&& id -> (p2, p))
          | size p == size p2 = p
          | otherwise = go p2

-- First we split on the direct outputs
initialPartition :: (Ord s, Ord o) => (s -> o) -> [s] -> Partition s
initialPartition o p = privTo $ groupWith o p

-- Then every node has a color, the relations then define the next-color
-- We split on the next-colors
refine :: Ord s => [s -> s] -> Partition s -> Partition s
refine ls p = privTo $ concatMap part (privFrom p)
  where
    colors s = [color p (f s) | f <- ls]
    part ss = groupWith colors ss
