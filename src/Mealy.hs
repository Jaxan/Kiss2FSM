{-# LANGUAGE OverloadedStrings #-}

module Mealy where

import           Data.ByteString (ByteString)
import           Data.Map.Strict (Map, adjust, empty, fromList, insert)
import           Data.Maybe
import           Data.Text.Lazy  (Text, pack, split)
import           ReadDot         (Command (N, E), parseDot)
import           Text.Parsec     (ParseError, parse)

type Mealy s i o = Map s (Map i (s, o))
type State = String
type Input = Text
type Output = Text

conv :: Command -> Maybe (State, Input, Output, State)
conv (E fromNode toNode attrs) = do
  firstLabel <- getOne $ mapMaybe getLabel attrs
  (input, output) <- getTwo $ split (== '/') firstLabel
  return (fromNode, input, output, toNode)
  where
    getLabel ("label", l) = Just (pack l); getLabel _ = Nothing
    getTwo [x,y] = Just (x, y); getTwo _ = Nothing
    getOne [x] = Just x; getOne _ = Nothing
conv _ = Nothing

convert :: [Command] -> Mealy State Input Output
convert graph = foldr handleEdge startMap edges
  where
    nodes = mapMaybe getNode graph
    getNode (N n _) = Just n; getNode _ = Nothing
    startMap = fromList (map (\x -> (x, empty)) nodes)
    edges = mapMaybe conv graph
    addEdge i s2 o = insert i (s2, o)
    handleEdge (s, i, o, s2) = adjust (addEdge i s2 o) s

parseMealy :: ByteString -> Either ParseError (Mealy State Input Output)
parseMealy txt = convert . snd <$> Text.Parsec.parse parseDot "" txt
