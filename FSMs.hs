{-# LANGUAGE PatternGuards, ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

import Circuit
import Dot
import Kiss
import Patterns

import Control.Exception (catch, SomeException)
import Control.Monad (forM)
import Data.ByteString.Builder (hPutBuilder)
import Data.List (isPrefixOf)
import Data.List.Ordered (nubSort)
import Data.Set (toList)
import System.IO (stdout)
import System.Directory (getDirectoryContents)
import System.Environment (getArgs)
import Text.Parsec.String (parseFromFile)

main = do
  [p, f] <- getArgs
  convert p f

doAll path = do
  ls <- getDirectoryContents path
  let kisses = filter (isPrefixOf (reverse ".kiss2") . reverse) ls
  forM kisses (\f -> catch (convert path f) (handler f))
  where
    handler :: String -> SomeException -> IO ()
    handler f ex = putStrLn $ "Caught exception: " ++ f ++ ": " ++ show ex

convert :: String -> String -> IO ()
convert path filename = do
  Just (circuit, is, init) <- open path filename
  convertSem filename (sinkObservable circuit) is init
  convertSem filename (loopObservable circuit) is init
  convertSem filename (sinkHidden circuit) is init
  convertSem filename (loopHidden circuit) is init

convertSem f s is init0 = do
  let name = takeWhile (/= '.') . reverse . takeWhile (/= '/') . reverse $ f
  let init = canonicalState s init0
  let sts = toList $ reachability s is init
  catch (hPutBuilder stdout $ printCircuitAsDot name s sts is init) handler
  let s2 = minimal s sts is
  let sts2 = nubSort $ fmap (minimalState s2) sts
  let init2 = minimalState s2 init
  catch (hPutBuilder stdout $ printCircuitAsDot name s2 sts2 is init2) handler
  where
    handler :: SomeException -> IO ()
    handler ex = putStrLn $ "Caught exception: " ++ f ++ ": " ++ show ex

open :: String -> String -> IO (Maybe (Circuit, [InputPattern], State))
open path filename = do
  result <- parseFromFile parseKiss (path ++ filename)
  case result of
    Left error -> do
      print error
      return Nothing
    Right k@(Kiss _ ts) -> do
      let c = toCircuit k
      let is = expandPatterns $ map (\(a,_,_,_)->a) ts
      let init = fst $ initialState k
      return $ Just (c, is, init)
