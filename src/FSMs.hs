{-# LANGUAGE OverloadedStrings #-}

import Circuit
import Dot
import Kiss
import Patterns
import Reachability

import Data.ByteString.Builder (hPutBuilder)
import Data.List.Ordered (nubSort)
import Data.Monoid ((<>))
import Data.Set (toList)
import Data.Vector (replicate)
import Prelude hiding (replicate)
import System.Environment (getArgs)
import System.IO (withFile, IOMode(WriteMode))
import System.FilePath (splitFileName, (</>))
import Text.Parsec.String (parseFromFile)

main = do
  [file, outdir] <- getArgs
  let (_, f) = splitFileName file
  convert file f outdir

convert :: FilePath -> String -> FilePath -> IO ()
convert filename name outdir = do
  Just (circuit0, is, (init, someOut)) <- open filename
  let n = length someOut
  let circuit = reachabilityCircuit init circuit0

  let (meal1, f1) = addSink (replicate n $ '-') circuit
  let r1 = toList $ reachability (f1 init) (\s -> map (fst . meal1 s) is)
  let (m1, f1') = minimizeMealy r1 is meal1

  let (meal2, f2) = addLoops (replicate n $ '-') circuit
  let r2 = toList $ reachability (f2 init) (\s -> map (fst . meal2 s) is)
  let (m2, f2') = minimizeMealy r2 is meal2

  let (meal3, f3) = addOutputState (replicate n $ '0') set m1
  let r3 = toList $ reachability (f3 . f1' . f1 $ init) (\s -> map (fst . meal3 s) is)
  let (m3, f3') = minimizeMealy r3 is meal3

  let (meal4, f4) = addOutputState (replicate n $ '0') set m1
  let r4 = toList $ reachability (f4 . f2' . f2 $ init) (\s -> map (fst . meal4 s) is)
  let (m4, f4') = minimizeMealy r4 is meal4

  -- We will partition the four models in classes. Note that there are some
  -- constraints by construction. For example if 1=2, then also 3=4.
  -- If 1=2 and 1=3, then we know all four models are equivalent, but it
  -- 1=2 and 1/=3, then still we have 3=4. The case 1/=2 is a bit more subtle.
  let bisim12 = bisimilar is m1 (f1' . f1 $ init) m2 (f2' . f2 $ init)
  let bisim13 = bisimilar is m1 (f1' . f1 $ init) m3 (f3' . f3 . f1' . f1 $ init)
  case (bisim12, bisim13) of
    (True, True) -> do
      let b1 = "digraph 1_2_3_4 {\n" <> printTransitions (toTransitions m1 (f1' . f1 $ init) (map f1' r1) is) <> "}\n"
      withFile' "1_2_3_4.dot" WriteMode (flip hPutBuilder b1)
    (True, False) -> do
      let b1 = "digraph 1_2 {\n" <> printTransitions (toTransitions m1 (f1' . f1 $ init) (map f1' r1) is) <> "}\n"
      let b3 = "digraph 3_4 {\n" <> printTransitions (toTransitions m3 (f3' . f3 . f1' . f1 $ init) (map f3' r3) is) <> "}\n"
      withFile' "1_2.dot" WriteMode (flip hPutBuilder b1)
      withFile' "3_4.dot" WriteMode (flip hPutBuilder b3)
    (False, _) -> do
      -- If there is a difference in 1 and 2, then it must mean that there is
      -- a don't-care output (in the sink or self loops). And so 1 /= 3 and 2 /= 4.
      -- It may still happen that 3 == 4, I guess.
      let b1 = "digraph 1 {\n" <> printTransitions (toTransitions m1 (f1' . f1 $ init) (map f1' r1) is) <> "}\n"
      let b2 = "digraph 2 {\n" <> printTransitions (toTransitions m2 (f2' . f2 $ init) (map f2' r2) is) <> "}\n"
      withFile' "1.dot" WriteMode (flip hPutBuilder b1)
      withFile' "2.dot" WriteMode (flip hPutBuilder b2)
      let bisim34 = bisimilar is m3 (f3' . f3 . f1' . f1 $ init) m4 (f4' . f4 . f2' . f2 $ init)
      case bisim34 of
        True -> do
          let b3 = "digraph 3_4 {\n" <> printTransitions (toTransitions m3 (f3' . f3 . f1' . f1 $ init) (map f3' r3) is) <> "}\n"
          withFile' "3_4.dot" WriteMode (flip hPutBuilder b3)
        False -> do
          let b3 = "digraph 3 {\n" <> printTransitions (toTransitions m3 (f3' . f3 . f1' . f1 $ init) (map f3' r3) is) <> "}\n"
          let b4 = "digraph 4 {\n" <> printTransitions (toTransitions m4 (f4' . f4 . f2' . f2 $ init) (map f4' r4) is) <> "}\n"
          withFile' "3.dot" WriteMode (flip hPutBuilder b3)
          withFile' "4.dot" WriteMode (flip hPutBuilder b4)
  where
    withFile' filename mode act = do
      let outfile = outdir </> name ++ "_" ++ filename
      withFile outfile mode act
      putStr "wrote to file: "
      putStrLn outfile


data PointedOrd s = PointedOrd { point :: s, extract :: s } deriving Eq

-- only well defined if the points agree
instance Ord s => Ord (PointedOrd s) where
  compare p q = case (extract p == point p, extract q == point q) of
    (True, True) -> EQ
    (True, False) -> LT
    (False, True) -> GT
    (False, False) -> extract p `compare` extract q

toTransitions :: Ord s => Mealy s i o -> s -> [s] -> [i] -> [(s, s, i, o)]
toTransitions meal initial ss is = [(s, t, i, o) | s <- sortedSs, i <- is, (t, o) <- [meal s i]]
  where sortedSs = map extract . nubSort . map (PointedOrd initial) $ ss

open :: FilePath -> IO (Maybe (Circuit, [InputPattern], (State, OutputPattern)))
open filename = do
  result <- parseFromFile parseKiss filename
  case result of
    Left error -> do
      print error
      return Nothing
    Right k@(Kiss _ ts) -> do
      let c = toCircuit k
      let is = expandPatterns $ map (\(a,_,_,_) -> a) ts
      let init = initialState k
      return $ Just (c, is, init)
