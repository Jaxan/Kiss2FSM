{-# LANGUAGE OverloadedStrings #-}

module Kiss where

import Circuit
import Patterns
import Data.List.Ordered (nubSort)
import Data.Map.Strict as M (insertWith, fromList)

import Control.Applicative
import Text.Parsec.String (Parser)
import Text.Parsec (many1, satisfy, string, spaces, endBy, digit)
import Text.Parsec.Char (char)
import Data.ByteString.Char8 (ByteString, pack)
import Data.Char (isAlphaNum)
import Data.Vector as V (fromList)

{-
We cannot yet handle Wildcard next states
There is only one specification where this occurs, afaik
So for now, I ignore it.
-}

data StateFormat = State State | Wildcard
  deriving (Show, Read, Eq, Ord)
type Key = (ByteString, Int)
type Rule = (Pattern, StateFormat, State, Pattern)
data KissFormat = Kiss [Key] [Rule]
  deriving (Show, Read, Eq, Ord)

getStates :: [Rule] -> [State]
getStates l = nubSort . concat . map getIt $ l
  where
    getIt (_,Wildcard,x,_) = [x]
    getIt (_,State y,x,_)  = [x,y]

toCircuit :: KissFormat -> Circuit
toCircuit (Kiss _ ts) = foldr update xempty ts
  where
    xempty = M.fromList (zip states (repeat []))
    states = getStates ts
    singleUpdate s p s2 p2 = insertWith (++) s [(p, (s2, p2))]
    update (p, State s, s2, p2) = singleUpdate s p s2 p2
    update (p, Wildcard, s2, p2) = foldMap (\s -> singleUpdate s p s2 p2) states

-- Gives state and initial output bits
-- Kind of a guess
initialState :: KissFormat -> (State, Pattern)
initialState (Kiss _ []) = undefined
initialState (Kiss _ ((_, Wildcard, st2, o2):_)) = (st2, o2)
initialState (Kiss _ ((_, State st1, _, o2):_)) = (st1, fmap (const '0') o2)
    
  
parseState :: Parser State
parseState = pack <$> (many1 $ satisfy (liftA2 (||) isAlphaNum isAllowed))
  where isAllowed c = c == '.' || c == '_'

parseInt :: Parser Int
parseInt = read <$> many1 digit

parseStateFormat :: Parser StateFormat
parseStateFormat = (Wildcard <$ string "*") <|> (State <$> parseState)

parsePattern :: Parser Pattern
parsePattern = V.fromList <$> many1 (satisfy digit)
  where digit c = c == '0' || c == '1' || c == '-'

parseLine :: Parser (Pattern, StateFormat, State, Pattern)
parseLine = (,,,) <$> parsePattern <* spaces <*> parseStateFormat <* spaces <*> parseState <* spaces <*> parsePattern

parseKey :: Parser (ByteString, Int)
parseKey = (,) <$> (char '.' *> parseState) <* spaces <*> parseInt

parseKiss :: Parser KissFormat
parseKiss = Kiss <$> (spaces *> (parseKey `endBy` spaces)) <*> (parseLine `endBy` spaces)


validate :: KissFormat -> Bool
validate (Kiss _ []) = False
validate (Kiss keys l) =
  allEqual i (map (length . \(a,_,_,_) -> a) l)
  && allEqual o (map (length . \(_,_,_,a) -> a) l)
  && equal p (length l)
  && equal s (length . getStates $ l)
  where
    i = lookup "i" keys
    o = lookup "o" keys
    p = lookup "p" keys
    s = lookup "s" keys
    allEqual Nothing []      = undefined
    allEqual Nothing (x:xs)  = allEqual (Just x) xs
    allEqual (Just x) []     = True
    allEqual (Just x) (y:xs) = x == y && allEqual (Just x) xs
    equal Nothing _  = True
    equal (Just x) y = x == y
