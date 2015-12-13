{-# LANGUAGE ViewPatterns #-}

module Minimization where

import Control.Arrow ((&&&))
import GHC.Exts (groupWith)

-- Naive data structure
type Partition s = [(Int, [s])]
type Class = Int

-- "color" / class of a state in the partition
-- It is only well defined if the element occurs in the partition
color :: (Eq s) => Partition s -> s -> Class
color ~((y, ss):ys) x = if elem x ss then y else color ys x

-- The kind-of inverse of color, picks a representive from a class
representative :: Partition s -> Class -> s
representative p i = head . snd $ p !! i

-- Number of classes
size :: Partition s -> Int
size p = length p

-- Here is the magic. Simple partition refinement. Not really efficient.
-- But good enough in this package. The Eq constraint is used to search
-- for a state in our data structre. The Ord constraint is used to
-- discriminate outputs. Ideally, we would use the discrimination package
-- for both of these things.
minimize :: (Eq s, Ord o) => [s] -> (s -> o) -> [s -> s] -> Partition s
minimize states output relations = go $ initialPartition output states
  where go (refine relations &&& id -> (p2, p))
          | size p == size p2 = p
          | otherwise = go p2

-- First we split on the direct outputs
initialPartition :: Ord o => (s -> o) -> [s] -> Partition s
initialPartition o p = priv_to $ groupWith o p

-- Then every node has a color, the relations then define the next-color
-- We split on the next-colors
refine :: Eq s => [s -> s] -> Partition s -> Partition s
refine ls p = priv_to $ concatMap part (priv_from p)
  where
    colors s = [color p (f s) | f <- ls]
    part ss = groupWith colors ss

priv_to p = zip [0..] p
priv_from p = map snd p
