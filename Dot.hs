{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}

module Dot where

import Circuit
import Patterns

import Data.List.Ordered
import Data.Maybe
import Data.Vector hiding ((++), and)

class DotPrintable a where
  dotPrint :: a -> String
  shouldPrint :: a -> Bool

instance DotPrintable State where
  dotPrint s = s
  shouldPrint = const True

instance DotPrintable Pattern where
  dotPrint = toList
  shouldPrint = const True

instance (DotPrintable x) => DotPrintable (Maybe x) where
  dotPrint (Just x) = dotPrint x
  dotPrint Nothing = "SINK_STATE"
  shouldPrint = isJust

instance (DotPrintable x, DotPrintable y) => DotPrintable (x, y) where
  dotPrint (x, y) = dotPrint x ++ dotPrint y
  shouldPrint (x, y) = shouldPrint x && shouldPrint y

instance (DotPrintable x) => DotPrintable (MinimalS x) where
  dotPrint (MinimalS x) = dotPrint x
  shouldPrint (MinimalS x) = shouldPrint x

type Trans s i o = (s, s, i, o)

instance (DotPrintable s, DotPrintable i, DotPrintable o) => DotPrintable (Trans s i o) where
  dotPrint (s, s2, i, o) = dotPrint s ++ " -> " ++ dotPrint s2 ++ " [label=\"" ++ dotPrint i ++ "/" ++ dotPrint o ++ "\"]"
  shouldPrint (s, s2, i, o) = shouldPrint s && shouldPrint i

printTransitions :: (DotPrintable s, DotPrintable i, DotPrintable o) => [Trans s i o] -> String
printTransitions ls = unlines [dotPrint l | l <- ls, shouldPrint l]

putInitialFirst :: (Eq s) => [s] -> s -> [s]
putInitialFirst ls i = sortOn (not . (i ==)) ls

printCircuitAsDot :: (Sem c, Ord (ExpandedState c), DotPrintable (ExpandedState c))
  => String -> c -> [ExpandedState c] -> [InputPattern] -> ExpandedState c -> String
printCircuitAsDot externalName c sts is init = "digraph " ++ externalName ++ name c ++ " {\n" ++ printTransitions allTrans ++ "}\n"
  where
    initSts = putInitialFirst sts init
    allTrans = [(s, st2, i, o) | s <- initSts, i <- is, (st2, o) <- beh c s i]
