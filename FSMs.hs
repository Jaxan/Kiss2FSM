{-# LANGUAGE PatternGuards, ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

import Circuit
import Kiss
import Patterns

import Control.Monad
import Data.List
import Data.Set (toList)
import System.Directory
import Text.Parsec

path = "./examples/"

main = doAll path

doAll path = do
  putStrLn showHeader
  ls <- getDirectoryContents path
  let kisses = filter (isPrefixOf (reverse ".kiss2") . reverse) ls
  forM kisses (\f -> do
    mr <- yolo path f
    case mr of
      Nothing -> putStr ""
      Just r -> print r)


yolo path filename = do
  file <- readFile (path ++ filename)
  let p = parse parseKiss filename file
  case p of
    Left error -> do
      print error
      return Nothing
    Right k -> do
      return . Just $ analyse filename k

data Result = Result
  { filename :: String
  , deterministic :: Bool
  , complete :: Bool
  , originalStates :: Maybe Int
  , inputBits :: Maybe Int
  , outputBits :: Maybe Int
  , expandedInputs :: Int
  , reachableStates :: Maybe Int
  , reachableFullStates :: Maybe Int
  }

showHeader = "filename\tdeterministic\tcomplete\toriginalStates\tinputBits\toutputBits\texpandedInputs\treachableStates\treachableFullStates"

instance Show Result where
  show (Result {..}) = take 10 (filename ++ repeat ' ')
    ++ "\t" ++ show deterministic
    ++ "\t" ++ show complete
    ++ "\t" ++ p originalStates
    ++ "\t" ++ p inputBits
    ++ "\t" ++ p outputBits
    ++ "\t" ++ show expandedInputs
    ++ "\t" ++ p reachableStates
    ++ "\t" ++ p reachableFullStates
    where
      p Nothing = "?"
      p (Just i) = show i

analyse :: String -> KissFormat -> Result
analyse f k@(Kiss keys ts)
  | ps <- map (\(a,_,_,_)->a) ts
  , is <- expandPatterns ps
  , c <- toCircuit k
  , (st1, op1) <- initialState k
  , oc <- Obs c
  , (toList -> ostates,_) <- reachability oc is st1
  , nc <- Nobs c
  , (toList -> nstates,_) <- reachability nc is (st1, op1)
  = Result
    { filename = f
    , deterministic = isDeterministic oc ostates is
    , complete = isComplete oc ostates is
    , inputBits = lookup "i" keys
    , outputBits = lookup "o" keys
    , originalStates = lookup "s" keys
    , expandedInputs = length is
    , reachableStates = if isComplete oc ostates is && isDeterministic oc ostates is then Just $ numberOfStates oc is st1 else Nothing 
    , reachableFullStates = if isComplete nc nstates is && isDeterministic nc nstates is then Just $ numberOfStates nc is (st1, op1) else Nothing 
    }
