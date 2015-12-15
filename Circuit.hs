{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Circuit where
import Patterns
import Minimization

import Prelude hiding (lookup)
import Data.List.Ordered (nubSort)
import Data.Map.Strict (Map, lookup, elems)
import Data.Set (Set, empty, member, insert, singleton, toList)

type InputPattern = Pattern
type OutputPattern = Pattern
type Circuit = Map State [(InputPattern, (State, OutputPattern))]


-- Semantics is given by an actual state type, and a transition function
class Sem a where
  type ExpandedState a
  beh :: a -> ExpandedState a -> InputPattern -> [(ExpandedState a, OutputPattern)]
  outputBits :: a -> Int

-- Collects all reachable statesfrom a start states
reachability :: (Sem a, Ord (ExpandedState a)) => a -> [InputPattern] -> ExpandedState a -> Set (ExpandedState a)
reachability c is st = dfs c is st (singleton st)
  where
    dfs c is st stAcc = foldr update stAcc [b | i <- is, b <- beh c st i]
    update (st2, o2) stAcc = case member st2 stAcc of
      True -> stAcc
      False -> dfs c is st2 (insert st2 stAcc)

-- Generalisation of the checks isDeterministic and isComplete.
isSomething :: (Sem a) => ([(ExpandedState a, OutputPattern)] -> Bool) -> a -> [ExpandedState a] -> [InputPattern] -> Bool
isSomething f c states is = all check states
  where
    checkOne state i = f (beh c state i)
    check state = all (checkOne state) is

-- Returns true if the behavriour is deterministic
isDeterministic :: (Sem a) => a -> [ExpandedState a] -> [InputPattern] -> Bool
isDeterministic = isSomething (\b -> length b <= 1)

-- Returns true if the behavriour is always defined
isComplete :: (Sem a) => a -> [ExpandedState a] -> [InputPattern] -> Bool
isComplete = isSomething (not . null)

-- Fused the two checks above
isDeterministicAndComplete :: (Sem a) => a -> [ExpandedState a] -> [InputPattern] -> Bool
isDeterministicAndComplete = isSomething isSingleton
  where isSingleton [_] = True; isSingleton _ = False

-- We can convert to the format used for minimization
-- Maybe we shouldn't use an initial state here
-- Minimization does not need an initial state anyways
type Carrier a = [ExpandedState a]
type Output a = ExpandedState a -> [OutputPattern]
type Transitions a = [ExpandedState a -> ExpandedState a]
toMealy :: (Sem a, Ord (ExpandedState a)) => a -> [ExpandedState a] -> [InputPattern] -> (Carrier a, Output a, Transitions a)
toMealy c carrier is = (carrier, output, transitions)
  where
    output = \s -> map (\i -> snd $ behaviour s i) is
    transitions = map (\i s -> fst $ behaviour s i) is
    behaviour st i = case beh c st i of
      [] -> error "Not defined"
      [x] -> x
      _ -> error "Non det mealy"

-- Minimized number of states. Would be an easy composition is we had uncurry3
numberOfStates circuit states is = size . (\(a,b,c) -> minimize a b c) $ toMealy circuit states is


-- Some semantics
base = Base
completeBase = Compl . base
hiddenStates = Nobs . base
completeHiddenStates = Nobs . completeBase


-- Observable: we can observe the don't-care bits '-'
newtype Base = Base Circuit
  deriving Show

instance Sem Base where
  type ExpandedState Base = State
  beh (Base c) st p = case lookup st c of
    Nothing -> error $ "No such state: " ++ st
    Just l -> nubSort [next | (i, next) <- l, p `fits` i]
  outputBits (Base c) = length . snd . snd . head . concat . elems $ c

-- Completion: add self loops with don't-care output to complete the machine
newtype Compl b = Compl b
  deriving Show

instance Sem b => Sem (Compl b) where
  type ExpandedState (Compl b) = ExpandedState b
  beh (Compl b) st p = case beh b st p of
    [] -> [(st, matchAll (outputBits b))]
    x -> x
  outputBits (Compl b) = outputBits b

-- Non observable: we cannot observe '-', instead, those bits will remain the same
newtype Nobs b = Nobs b
  deriving Show

instance (Sem b, Ord (ExpandedState b)) => Sem (Nobs b) where
  type ExpandedState (Nobs b) = (ExpandedState b, OutputPattern)
  beh (Nobs b) (st, o1) p = nubSort [((st2, o1 `set` o2), o1 `set` o2) | (st2, o2) <- beh b st p]
  outputBits (Nobs b) = outputBits b
