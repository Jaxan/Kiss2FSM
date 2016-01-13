{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

module Mealy where

import           Control.Monad.Writer
import           Data.ByteString         (ByteString)
import           Data.ByteString.Builder
import           Data.Map.Strict         as M (Map, adjust, empty, foldr,
                                               fromList, insert, keys, keysSet,
                                               lookup)
import           Data.Maybe              (fromJust, mapMaybe)
import           Data.Set                as S (empty, toList, union)
import           Minimization
import           ReadDot                 (Command (N, E), parseDot)
import           Text.Parsec             (ParseError, parse)

-- Type class describing a mealy machine abstractly.
-- Note that i o are also part of the signature, so that specialized version
-- which only accept certain types can exist (for example for Ints with direct
-- indexing). The state space can also be a extra parameter, but I think that
-- would be a fundep, so I made it a type family.
class Mealy m i o where
  type State m i o
  states :: m i o -> [State m i o]
  inputs :: m i o -> [i]
  beh :: m i o -> State m i o -> i -> (State m i o, o)


-- The easiest kind representation is one with a Map
-- This could also be represented as Map (s, i) (s, 0)
-- But I choose the curried one for no reason.
newtype MapMealy s i o = MapMealy (Map s (Map i (s, o)))

instance (Ord s, Ord i) => Mealy (MapMealy s) i o where
  type State (MapMealy s) i o = s
  states (MapMealy m) = keys m
  inputs (MapMealy m) = S.toList . M.foldr S.union S.empty . fmap keysSet $ m
  beh (MapMealy m) s i = ulookup i (ulookup s m)
    where ulookup k m = fromJust $ M.lookup k m


-- The more interesting one is the Minimized version. Any mealy machine can
-- be minimized, and we obtain a new mealy machine, where states are given
-- by classes of states. So this data type keeps the original machine and a
-- partition of states.
-- I put the Ord instance for the state space in this record, because
-- otherwise I would need UndecidableInstances in the Mealy instance defined
-- below. This way it shows to be safe.
data Minimized m i o = Minimized { machine :: m i o, partition :: Partition (State m i o), ordInstance :: OrdDict (State m i o) }

instance (Mealy m i o) => Mealy (Minimized m) i o where
  type State (Minimized m) i o = Class
  states Minimized{..} = [0..size partition - 1]
  inputs Minimized{..} = inputs machine
  beh Minimized{..} cls i = (reify ordInstance color partition s2, o)
    where (s2, o) = beh machine (representative partition cls) i

-- This function takes a mealy machine and minimizes it
minimizeMealy :: (Mealy m i o, Ord o, Ord (State m i o)) => m i o -> Minimized m i o
minimizeMealy mealy = Minimized mealy partition OrdDict
  where
    outputs s = fmap (snd . beh mealy s) (inputs mealy)
    transitions = fmap (\i s -> fst $ beh mealy s i) (inputs mealy)
    partition = minimize (states mealy) outputs transitions

-- This is the magic, making the Ord-thingies possibles
data OrdDict a where OrdDict :: Ord a => OrdDict a
reify :: OrdDict a -> (Ord a => t) -> t
reify OrdDict t = t


-- Below is writing mealy machine into dot files
class Write a where
  write :: a -> Builder

instance Write String where write = stringUtf8
instance Write Int where write = write . show

writeMealyToDot :: (Mealy m i o, Write (State m i o), Write i, Write o) => m i o -> Builder
writeMealyToDot mealy =
  "digraph {\n"
  <> foldMap (\s -> write s <> "\n") (states mealy)
  <> foldMap (\(s, i, o, s2) -> write s <> " --> " <> write s2 <> " [label=\"" <> write i <> "/" <> write o <> "\"]\n") edges
  <> "}\n"
  where
    domain = (,) <$> states mealy <*> inputs mealy
    edges = [(s, i, snd $ beh mealy s i, fst $ beh mealy s i) | (s, i) <- domain]


-- Below is parsing of dot files into mealy machines
type MMState = String
type MMInput = String
type MMOutput = String
parseMealy :: ByteString -> Either ParseError (MapMealy MMState MMInput MMOutput)
parseMealy txt = convert . snd <$> Text.Parsec.parse parseDot "" txt

convert :: [Command] -> MapMealy MMState MMInput MMOutput
convert graph = MapMealy $ Prelude.foldr handleEdge startMap edges
  where
    nodes = concatMap getNodes graph
    getNodes (N n _) = [n]; getNodes (E n1 n2 _) = [n1, n2]
    startMap = fromList (map (\x -> (x, M.empty)) nodes)
    edges = mapMaybe conv graph
    addEdge i s2 o = insert i (s2, o)
    handleEdge (s, i, o, s2) = adjust (addEdge i s2 o) s

conv :: Command -> Maybe (String, String, String, String)
conv (E fromNode toNode attrs) = do
  firstLabel <- getOne $ mapMaybe getLabel attrs
  (input, output) <- getTwo $ split '/' firstLabel
  return (fromNode, input, output, toNode)
  where
    getLabel ("label", l) = Just l; getLabel _ = Nothing
    getTwo [x,y] = Just (x, y); getTwo _ = Nothing
    getOne [x] = Just x; getOne _ = Nothing
conv _ = Nothing

split :: Char -> String -> [String]
split c s = cont $ span (/= c) s
  where
    cont (str, []) = [str]
    cont (str, rest) = str:split c (tail rest)
