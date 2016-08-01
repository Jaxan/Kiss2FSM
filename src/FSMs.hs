{-# LANGUAGE OverloadedStrings #-}

import Circuit
import Dot
import Kiss
import Patterns
import Reachability

import Data.ByteString.Builder (hPutBuilder, byteString)
import Data.Monoid ((<>))
import Data.Set (toList)
import Data.Vector (replicate)
import Prelude hiding (replicate)
import System.Environment (getArgs)
import System.IO (stdout)
import Text.Parsec.String (parseFromFile)

main = do
  [p, f] <- getArgs
  convert p f

convert :: String -> String -> IO ()
convert path filename = do
  Just (circuit0, is, (init, someOut)) <- open path filename
  let n = length someOut
  let circuit = reachabilityCircuit init circuit0

  let (meal1, f1) = addSink (replicate n $ '-') circuit
  let r1 = toList $ reachability (f1 init) (\s -> map (fst . meal1 s) is)
  let (m1, f1') = minimizeMealy r1 is meal1
  let b1 = byteString "bla 1" <> printTransitions (toTransitions m1 (map f1' r1) is)
  hPutBuilder stdout b1

  let (meal2, f2) = addLoops (replicate n $ '-') circuit
  let r2 = toList $ reachability (f2 init) (\s -> map (fst . meal2 s) is)
  let (m2, f2') = minimizeMealy r2 is meal2
  let b2 = byteString "bla 2" <> printTransitions (toTransitions m2 (map f2' r2) is)
  hPutBuilder stdout b2

  let (meal3, f3) = addOutputState (replicate n $ '0') set m1
  let r3 = toList $ reachability (f3 . f1' . f1 $ init) (\s -> map (fst . meal3 s) is)
  let (m3, f3') = minimizeMealy r3 is meal3
  let b3 = byteString "bla 3" <> printTransitions (toTransitions m3 (map f3' r3) is)
  hPutBuilder stdout b3

  let (meal4, f4) = addOutputState (replicate n $ '0') set m1
  let r4 = toList $ reachability (f4 . f2' . f2 $ init) (\s -> map (fst . meal4 s) is)
  let (m4, f4') = minimizeMealy r4 is meal4
  let b4 = byteString "bla 4" <> printTransitions (toTransitions m4 (map f4' r4) is)
  hPutBuilder stdout b4

toTransitions :: Mealy s i o -> [s] -> [i] -> [(s, s, i, o)]
toTransitions meal ss is = [(s, t, i, o) | s <- ss, i <- is, (t, o) <- [meal s i]]

open :: String -> String -> IO (Maybe (Circuit, [InputPattern], (State, OutputPattern)))
open path filename = do
  result <- parseFromFile parseKiss (path ++ filename)
  case result of
    Left error -> do
      print error
      return Nothing
    Right k@(Kiss _ ts) -> do
      let c = toCircuit k
      let is = expandPatterns $ map (\(a,_,_,_) -> a) ts
      let init = initialState k
      return $ Just (c, is, init)
