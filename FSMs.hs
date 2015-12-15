{-# LANGUAGE PatternGuards, ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

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

analyseSem sem init is
  | sts <- toList $ reachability sem is init
  , valid <- isDeterministicAndComplete sem sts is
  = case valid of
      True -> Just $ numberOfStates sem sts is
      False -> Nothing

isNothing Nothing = True
isNothing _ = False

analyse :: String -> KissFormat -> Result
analyse f k@(Kiss keys ts)
  | ps <- map (\(a,_,_,_)->a) ts
  , is <- expandPatterns ps
  , c <- toCircuit k
  , (st1, op1) <- initialState k
  , n1 <- analyseSem (base c) (st1) is
  , n2 <- analyseSem (completeBase c) (st1) is
  , n3 <- analyseSem (hiddenStates c) (st1, op1) is
  , n4 <- analyseSem (completeHiddenStates c) (st1, op1) is
  = Result
    { filename = f
    , deterministic = True
    , complete = not $ isNothing n1
    , inputBits = lookup "i" keys
    , outputBits = lookup "o" keys
    , originalStates = lookup "s" keys
    , expandedInputs = length is
    , reachableStates = if isNothing n1 then n2 else n1
    , reachableFullStates = if isNothing n3 then n4 else n3
    }
