{-# LANGUAGE DeriveFunctor #-}

module Patterns where

import Control.Arrow ((&&&))
import Data.List (tails)
import Data.List.Ordered (nubSortOn)
import Data.Set (member, union, empty, insert)
import Data.Vector (zipWith, and, all, filter, replicate, length, Vector)
import Data.ByteString (ByteString)
import Prelude hiding (zipWith, and, all, filter, replicate, length)

type State = ByteString
type Pattern = Vector Char

-- Basics per bit
compa x y = x == '-' || y == '-' || x == y   -- compatible bits (symmetric)
fit x y   = y == '-' || x == y               -- x fits in the bit y (order)
defi c    = c == '0' || c == '1'             -- defined bits

-- Extension to patterns
compatible xs ys = and $ zipWith compa xs ys -- e.g. "-0-" `compatible` "1--"
fits xs ys       = and $ zipWith fit xs ys   -- e.g. "011" `fits` "---"
defined xs       = all defi xs               -- e.g. "011", but not "-01"

-- definiteness of a sequences, useful for (topological) sorting
grade = length . filter defi
-- Consider the function: sortOn grade
-- This will sort the list with a topological sort (can be made linear, if we
-- really want to). More explicitly, if x is more specific than y, then y comes
-- before x in the sorted list. This enables greedy algorithms.


-- Compatible bits can be composed to the more specific one
comb x y
  | x == '-'  = y
  | y == '-'  = x
  | x == y    = x
  | otherwise = error (x:y:" not unifiable") 

-- Extension to sequences. Note that this is a (partial) commutative monoid.
-- The result is compatible with both arguments. matchAll is the neutral element
combine xs ys = zipWith comb xs ys
matchAll n = replicate n '-'


-- One can reset bits, so a '-' leaves the bit as is, otherwise sets it
-- This forms a (partial) non-commutative monoid, with matchAll as neutral element
st x y
  | y == '-' = x
  | otherwise = y

set xs ys = zipWith st xs ys


-- Derivation tree of all necessary sequences. I guess, we could inline
-- everything with folds and such (fusing dfs with leaves). But this was easier
-- for me.
data Tree a = Node [Tree a] a
  deriving (Show, Read, Eq, Ord, Functor)


-- It is created with a depth first search, siblings of a node cannot be
-- constructed with elements used "on the left".
dfs :: [Pattern] -> Tree Pattern
dfs [] = undefined
dfs xs = snd $ makechild root root xs
  where
    n = length (head xs)
    -- The top node has the pattern "----"
    root = matchAll n
    -- All distinct choices one can make on a certain level
    -- We will remove already used patterns later
    choices parent patterns = [(pat,t) | pat:t <- tails patterns, compatible pat parent]
    -- This will accumulate used patterns and make a node for every choice
    step parent (used, siblings) (choice, t) = if choice `member` used
      then (used, siblings)
      else let (s, c) = makechild parent choice t in (used `union` s, c : siblings)
    -- Using the accumulation function, we can fold the choices, creating children
    children parent patterns = foldl (step parent) (empty, []) (choices parent patterns)
    -- This one actually makes a node.
    makechild parent choice t = let (s, cs) = children (combine choice parent) t in (choice `insert` s, Node cs (combine choice parent))

-- The leaves of the derivation tree will contain the necessary elements
leaves :: Tree a -> [a]
leaves (Node [] a) = [a]
leaves (Node xs _) = concatMap leaves xs

-- Combining all the above. From a list of patterns, construct the necessary
-- combinations. Note that it can happen that there are still combinations which
-- are redundant, but for this we need the fsm-structure.
expandPatterns :: [Pattern] -> [Pattern]
expandPatterns xs = leaves . dfs . nubSortOn (grade &&& id) $ xs
