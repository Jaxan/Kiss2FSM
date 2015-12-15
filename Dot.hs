{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Dot where

import Circuit
import Patterns

import Data.ByteString.Char8
import Data.ByteString.Builder
import Data.List.Ordered
import Data.Maybe
import Data.Monoid ((<>))
import Data.Vector hiding ((++), and)
import Prelude hiding (unlines)

class DotPrintable a where
  dotPrint :: a -> Builder
  shouldPrint :: a -> Bool

instance DotPrintable State where
  dotPrint s = byteString s
  shouldPrint = const True

instance DotPrintable Pattern where
  dotPrint = byteString . pack . toList
  shouldPrint = const True

instance (DotPrintable x) => DotPrintable (Maybe x) where
  dotPrint (Just x) = dotPrint x
  dotPrint Nothing = "SINK_STATE"
  shouldPrint = isJust

instance (DotPrintable x, DotPrintable y) => DotPrintable (x, y) where
  dotPrint (x, y) = dotPrint x <> dotPrint y
  shouldPrint (x, y) = shouldPrint x && shouldPrint y

instance (DotPrintable x) => DotPrintable (MinimalS x) where
  dotPrint (MinimalS x) = dotPrint x
  shouldPrint (MinimalS x) = shouldPrint x

type Trans s i o = (s, s, i, o)

instance (DotPrintable s, DotPrintable i, DotPrintable o) => DotPrintable (Trans s i o) where
  dotPrint (s, s2, i, o) = dotPrint s <> " -> " <> dotPrint s2 <> " [label=\"" <> dotPrint i <> "/" <> dotPrint o <> "\"]"
  shouldPrint (s, s2, i, o) = and [shouldPrint s, shouldPrint s2, shouldPrint i, shouldPrint o]

printTransitions :: (DotPrintable s, DotPrintable i, DotPrintable o) => [Trans s i o] -> Builder
printTransitions ls = mconcat [dotPrint l <> "\n" | l <- ls, shouldPrint l]

putInitialFirst :: (Eq s) => [s] -> s -> [s]
putInitialFirst ls i = sortOn (not . (i ==)) ls

printCircuitAsDot :: (Sem c, Ord (ExpandedState c), DotPrintable (ExpandedState c))
  => String -> c -> [ExpandedState c] -> [InputPattern] -> ExpandedState c -> Builder
printCircuitAsDot externalName c sts is init = "digraph " <> (byteString . pack $ externalName) <> name c <> " {\n" <> printTransitions allTrans <> "}\n"
  where
    initSts = putInitialFirst sts init
    allTrans = [(s, st2, i, o) | s <- initSts, i <- is, (st2, o) <- beh c s i]
