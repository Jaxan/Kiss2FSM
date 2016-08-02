module Reachability where

import Data.Set (Set, empty, insert, member)

reachability :: Ord s => s -> (s -> [s]) -> Set s
reachability init trans = go init empty
    where
        go s acc
            | s `member` acc = acc
            | otherwise      = let acc2 = insert s acc in
                               let nsuccs = filter (not . flip member acc2) (trans s) in
                               foldr (\t acc3 -> go t acc3) acc2 nsuccs

