module Circuit where

import Minimization
import Patterns
import Reachability

import Data.List.Ordered (nubSort)
import Data.Map.Strict (Map, lookup, fromSet, intersection)
import Prelude hiding (lookup)

-- Invariant: All circuits are deterministic (which is the case)
-- but they might have a partial transition structure
-- so we define multiple ways of making it complete

type InputPattern = Pattern
type OutputPattern = Pattern
type Circuit = Map State [(InputPattern, (State, OutputPattern))]

-- We model mealy machine coalgebraically, this makes minimization easy
-- The initial state needs to be remembered separately
type Mealy s i o = s -> i -> (s, o)
-- For the initial states (or any other states), we need a way to map
-- into the constructed Mealy machine
type Include s t = s -> t

-- For the states without successors we define a sink state
-- This means that the machine 'crashed' in some sink state
-- The output is '----...', i.e. a don't-care output
addSink :: OutputPattern -> Circuit -> (Mealy (Maybe State) InputPattern OutputPattern, Include State (Maybe State))
addSink defaultOut circ = (behaviour, pure) where
  behaviour Nothing _ = (Nothing, defaultOut)
  behaviour (Just s) p = case lookup s circ of
    Nothing -> error "state does not exist"
    Just l  -> case nubSort [next | (i, next) <- l, p `fits` i] of
      []        -> (Nothing, defaultOut)
      [(s2, o)] -> (Just s2, o)
      _         -> error "too many successor states"

-- For the states without successors we can also define self-loops
-- Here the machine does not 'crash' but simply ignores the partiality
-- The output is '----...', i.e. a don't-care output
addLoops :: OutputPattern -> Circuit -> (Mealy State InputPattern OutputPattern, Include State State)
addLoops defaultOut circ = (behaviour, id) where
  behaviour s p = case lookup s circ of
    Nothing -> error "state does not exist"
    Just l  -> case nubSort [next | (i, next) <- l, p `fits` i] of
      []        -> (s, defaultOut)
      [(s2, o)] -> (s2, o)
      _         -> error "too many successor states"

-- So far the machine outputs don't care bits
-- More realistically a circuit has either 0 or 1 on its output bits
-- (and no '-'). So here we add the currently set output bits as state
-- Now a new output overwrites that, but leaves the bit fixed in case of
-- a don't care '-'
addOutputState :: o -> (o -> o -> o) -> Mealy s i o -> (Mealy (s, o) i o, Include s (s, o))
addOutputState defaultOut refine f = (behaviour, defaultState) where
  behaviour (s, o) i = let (s2, o2) = f s i; o3 = o `refine` o2 in ((s2, o3), o3)
  defaultState s = (s, defaultOut)

-- Function to shrink a circuit to its reachable part
reachabilityCircuit :: State -> Circuit -> Circuit
reachabilityCircuit s circ = circ `intersection` (fromSet (const ()) reachableStates)
  where
    reachableStates = reachability s succ
    succ t = case lookup t circ of
      Nothing -> []
      Just rules -> nubSort . map (fst . snd) $ rules

-- Function to obtain a minimized mealy machines
minimizeMealy :: (Ord s, Ord o) => [s] -> [i] -> Mealy s i o -> (Mealy Class i o, Include s Class)
minimizeMealy allStates inputs f = (\srep -> map1 toRep . f (fromRep srep), toRep)
  where
    partition = minimize allStates outpufF trans
    fromRep = representative partition
    toRep = color partition
    outpufF s = map (\i -> snd $ f s i) inputs
    trans = map (\i s -> fst $ f s i) inputs
    map1 f (a, b) = (f a, b)
