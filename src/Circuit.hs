{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Circuit where
import Patterns
import Minimization

import Prelude hiding (lookup, replicate)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Char8 (unpack)
import Data.List.Ordered (nubSort)
import Data.Map.Strict (Map, lookup, elems)
import Data.Monoid ((<>))
import Data.Set (Set, member, insert, singleton)
import Data.Vector (replicate)

type InputPattern = Pattern
type OutputPattern = Pattern
type Circuit = Map State [(InputPattern, (State, OutputPattern))]

-- Semantics is given by an actual state type, and a transition function
class Sem a where
  type ExpandedState a
  beh :: a -> ExpandedState a -> InputPattern -> [(ExpandedState a, OutputPattern)]
  canonicalState :: a -> State -> ExpandedState a
  outputBits :: a -> Int
  name :: a -> Builder

-- Collects all reachable statesfrom a start states
reachability :: (Sem a, Ord (ExpandedState a)) => a -> [InputPattern] -> ExpandedState a -> Set (ExpandedState a)
reachability c is st = dfs c is st (singleton st)
  where
    dfs c is st stAcc = foldr update stAcc [b | i <- is, b <- beh c st i]
    update (st2, _) stAcc = case member st2 stAcc of
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

-- Some semantics
sinkObservable = ComplSink . Base
loopObservable = Compl . Base
sinkHidden = ComplSink . Nobs . Base
loopHidden = Nobs . Compl . Base

minimal circuit states is = Minimal (circuit, (\(a,b,c) -> minimize a b c) $ toMealy circuit states is)

-- Observable: we can observe the don't-care bits '-'
newtype Base = Base Circuit
instance Sem Base where
  type ExpandedState Base = State
  beh (Base c) st p = case lookup st c of
    Nothing -> error $ "No such state: " ++ unpack st
    Just l -> nubSort [next | (i, next) <- l, p `fits` i]
  canonicalState _ st = st
  outputBits (Base c) = length . snd . snd . head . concat . elems $ c
  name _ = ""

-- Completion: add self loops with don't-care output to complete the machine
newtype Compl b = Compl b
instance Sem b => Sem (Compl b) where
  type ExpandedState (Compl b) = ExpandedState b
  beh (Compl b) st p = case beh b st p of
    [] -> [(st, matchAll (outputBits b))]
    x -> x
  canonicalState (Compl b) = canonicalState b
  outputBits (Compl b) = outputBits b
  name (Compl b) = name b <> "_with_loops"

-- Completion: add a sink state
newtype ComplSink b = ComplSink b
instance Sem b => Sem (ComplSink b) where
  type ExpandedState (ComplSink b) = Maybe (ExpandedState b)
  beh (ComplSink b) Nothing _  = [(Nothing, matchAll (outputBits b))]
  beh (ComplSink b) (Just s) p = case beh b s p of
    [] -> [(Nothing, matchAll (outputBits b))]
    x -> map (\(s, o) -> (Just s, o)) x
  canonicalState (ComplSink b) = Just . canonicalState b
  outputBits (ComplSink b) = outputBits b
  name (ComplSink b) = name b <> "_with_sink"

-- Non observable: we cannot observe '-', instead, those bits will remain the same
newtype Nobs b = Nobs b
instance (Sem b, Ord (ExpandedState b)) => Sem (Nobs b) where
  type ExpandedState (Nobs b) = (ExpandedState b, OutputPattern)
  beh (Nobs b) (st, o1) p = nubSort [((st2, o1 `set` o2), o1 `set` o2) | (st2, o2) <- beh b st p]
  canonicalState (Nobs b) st = (canonicalState b st, replicate (outputBits b) '0')
  outputBits (Nobs b) = outputBits b
  name (Nobs b) = name b <> "_with_hidden_states"

-- We can also wrap a minimal thingy as newtype :D
newtype Minimal b = Minimal (b, Partition (ExpandedState b))
newtype MinimalS s = MinimalS s deriving (Eq, Ord)
instance (Sem b, Ord (ExpandedState b)) => Sem (Minimal b) where
  type ExpandedState (Minimal b) = MinimalS (ExpandedState b)
  beh c@(Minimal (b, _)) (MinimalS s) i = nubSort [(minimalState c $ st2, o2) | (st2, o2) <- beh b s i]
  canonicalState c@(Minimal (b, _)) s = minimalState c $ canonicalState b s
  outputBits (Minimal (b, _)) = outputBits b
  name (Minimal (b, _)) = name b <> "_minimized"

minimalState (Minimal (_, p)) = MinimalS . representative p . color p
