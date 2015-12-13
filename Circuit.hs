{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module Circuit where

import Patterns
import Minimization

import Prelude hiding (lookup)
import Data.List.Ordered (nubSort)
import Data.Map.Strict (Map, lookup)
import Data.Set (Set, empty, member, insert, singleton, toList)

type InputPattern = Pattern
type OutputPattern = Pattern
type Circuit = Map State [(InputPattern, (State, OutputPattern))]


-- Semantics is given by an actual state type, and a transition function
class Sem a where
  type ExpandedState a
  beh :: a -> ExpandedState a -> InputPattern -> [(ExpandedState a, OutputPattern)]

-- Collects all reachable states (and possible outputs) from a start states
reachability :: (Sem a, Ord (ExpandedState a)) => a -> [InputPattern] -> ExpandedState a -> (Set (ExpandedState a), Set OutputPattern)
reachability c is st = dfs c is st (singleton st, empty)
  where
    dfs c is st (stAcc, oAcc) = foldr update (stAcc, oAcc) (concat [beh c st i | i <- is])
    update (st2, o2) (stAcc, outAcc) = case member st2 stAcc of
      True -> (stAcc, insert o2 outAcc)
      False -> dfs c is st2 (insert st2 stAcc, insert o2 outAcc)

-- Returns true if the behavriour is deterministic
isDeterministic :: (Sem a) => a -> [ExpandedState a] -> [InputPattern] -> Bool
isDeterministic c states is = all check states
  where
    checkOne state i = length (beh c state i) <= 1
    check state = all (checkOne state) is

-- Returns true if the behavriour is always defined
isComplete :: (Sem a) => a -> [ExpandedState a] -> [InputPattern] -> Bool
isComplete c states is = all check states
  where
    checkOne state i = length (beh c state i) >= 1
    check state = all (checkOne state) is

-- We can convert to the format used for minimization
-- Maybe we shouldn't use an initial state here
-- Minimization does not need an initial state anyways
type Carrier a = [ExpandedState a]
type Output a = ExpandedState a -> [OutputPattern]
type Transitions a = [ExpandedState a -> ExpandedState a]
toMealy :: (Sem a, Ord (ExpandedState a)) => a -> [InputPattern] -> ExpandedState a -> (Carrier a, Output a, Transitions a)
toMealy c is st = (carrier, output, transitions)
  where
    carrier = toList . fst $ reachability c is st
    output = \s -> map (\i -> snd $ behaviour s i) is
    transitions = map (\i s -> fst $ behaviour s i) is
    behaviour st i = case beh c st i of
      [] -> error "Not defined"
      [x] -> x
      _ -> error "Non det mealy"

numberOfStates circuit is st = size . (\(a,b,c) -> minimize a b c) $ toMealy circuit is st

-- Observable: we can observe the don't-care bits '-'
newtype Obs = Obs Circuit
  deriving Show

instance Sem Obs where
  type ExpandedState Obs = State
  beh (Obs c) st p = case lookup st c of
    Nothing -> error $ "No such state: " ++ st
    Just l -> nubSort [next | (i, next) <- l, p `fits` i]


-- Non observable: we cannot observe '-', instead, those bits will remain the same
newtype Nobs = Nobs Circuit
  deriving Show

instance Sem Nobs where
  type ExpandedState Nobs = (State, OutputPattern)
  beh (Nobs c) (st, o1) p = case lookup st c of
    Nothing -> error $ "No such state: " ++ st
    Just l -> nubSort [((st2, o1 `set` o2), o1 `set` o2) | (i, (st2, o2)) <- l, p `fits` i]
