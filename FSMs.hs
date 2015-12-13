{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}

import Circuit
import Kiss
import Patterns
import Minimization

import Control.Monad
import Data.List
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
    Right k@(Kiss keys ts) -> do
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
  , nd <- nondeterminism is c
  , pt <- incompleteness is c
  , (s1, o1, t1) <- toObservingMealy c is st1
  , (s2, o2, t2) <- toNonObservingMealy c is (st1, op1)
  , acceptable <- null nd && null pt
  = Result
    { filename = f
    , deterministic = null nd
    , complete = null pt
    , inputBits = lookup "i" keys
    , outputBits = lookup "o" keys
    , originalStates = lookup "s" keys
    , expandedInputs = length is
    , reachableStates = if acceptable then Just $ size (minimize s1 o1 t1) else Nothing 
    , reachableFullStates = if acceptable then Just $ size (minimize s2 o2 t2) else Nothing
    }

