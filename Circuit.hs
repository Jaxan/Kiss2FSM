module Circuit where

import Patterns
import Mealy

import Prelude hiding (lookup)
import Control.Monad (replicateM)
import Data.Monoid
import Data.List.Ordered (nubSort)
import Data.Map.Strict (Map, elems, mapWithKey, lookup, keys)
import Data.Set (Set, empty, member, union, singleton, toList)

type InputPattern = Pattern
type OutputPattern = Pattern
type Circuit = Map State [(InputPattern, (State, OutputPattern))]

{-
I should do some refactoring here
the pattern: nubSort . map snd . filter (fits i . fst)
is used a lot
-}

-- returns non deterministic states and patterns
-- nondeterminism :: [Pattern] -> Circuit -> [(State, Pattern)]
nondeterminism inputs m = concat . elems $ nonDetM
  where
    checkOne i s ts = nubSort . map snd . filter (fits i . fst) $ ts
    check s ts = [(s, i, checkOne i s ts) | i <- inputs, length (checkOne i s ts) > 1]
    nonDetM = mapWithKey check m

-- returns states and patterns for which nothing is defined
-- incompleteness :: [Pattern] -> Circuit -> [(State, Pattern)]
incompleteness inputs m = concat . elems $ incompM
  where
    checkOne i s ts = nubSort . map snd . filter (fits i . fst) $ ts
    check s ts = [(s, i, checkOne i s ts) | i <- inputs, length (checkOne i s ts) < 1]
    incompM = mapWithKey check m

-- We can observe '-' as output
observingBehaviour :: Circuit -> State -> InputPattern -> [(State, OutputPattern)]
observingBehaviour c st p = case lookup st c of
  Nothing -> error $ "No such state: " ++ st
  Just l -> nubSort . map snd . filter (fits p . fst) $ l

-- We cannot observe '-' as output, so it is 'hidden' state
nonObservingBehaviour :: Circuit -> State -> OutputPattern -> InputPattern -> [(State, OutputPattern)]
nonObservingBehaviour c st o1 p = case lookup st c of
  Nothing -> error $ "No such state: " ++ st
  Just l -> nubSort . map (\(st2, p2) -> (st2, o1 `set` p2)) . map snd . filter (fits p . fst) $ l


observingReachability :: Circuit -> [InputPattern] -> State -> Set State
observingReachability c is st = dfs c is st (singleton st)
  where
    dfs c is st acc = foldr update acc (concat [observingBehaviour c st i | i <- is])
    update (st2, _) acc = case member st2 acc of
      True -> acc
      False -> dfs c is st2 (acc `union` singleton st2)

nonObservingReachability :: Circuit -> [InputPattern] -> State -> OutputPattern -> Set (State, OutputPattern)
nonObservingReachability c is st o1 = dfs c is st o1 (singleton (st, o1))
  where
    dfs c is st o1 acc = foldr update acc (concat [nonObservingBehaviour c st o1 i | i <- is])
    update (st2, o2) acc = case member (st2, o2) acc of
      True -> acc
      False -> dfs c is st2 o2 (acc `union` singleton (st2, o2))


toObservingMealy :: Circuit -> [InputPattern] -> State -> Machine State InputPattern OutputPattern
toObservingMealy c is st = Machine (toList $ observingReachability c is st) is (get snd behaviour) (get fst behaviour)
  where
    get field fun = \s i -> field $ fun s i
    behaviour st i = case observingBehaviour c st i of
      [] -> error "Not defined"
      [x] -> x
      _ -> error "Non det mealy"
      
toNonObservingMealy :: Circuit -> [InputPattern] -> State -> OutputPattern -> Machine (State, OutputPattern) InputPattern OutputPattern
toNonObservingMealy c is st o1 = Machine (toList $ nonObservingReachability c is st o1) is (get snd behaviour) (get fst behaviour)
  where
    get field fun = \s i -> field $ fun s i
    behaviour (st, o1) i = case nonObservingBehaviour c st o1 i of
      [] -> error "Not defined"
      [(x, o2)] -> ((x, o1 `set` o2), o1 `set` o2)
      _ -> error "Non det mealy"















