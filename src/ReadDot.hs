{-# LANGUAGE OverloadedStrings #-}

module ReadDot where

{-
 I tried the graphviz package (with the Data.GraphViz module), but parsing
 was terribly slow. So I had to roll my own :-(. Much faster, but not as
 rich as Data.GraphViz.

 Known issues:
   Won't parse: Attributes with a '_' in the type (there are two)
   Won't parse: Values (of attributes) with '.', such as floating points
   Won't parse: Subgraphs / clusters
   Won't parse: Multiple edges at once: a -> b -> c
   Won't parse: Undirected graphs
   Parses attribute statement as node statements
   Doesn't handle escaped quotes
-}

import           Control.Arrow          ((&&&))
import           Data.Either            (lefts, rights)
import           Text.Parsec            (alphaNum, between, char, many, many1,
                                         noneOf, sepBy, space, spaces, string,
                                         try, (<|>))
import           Text.Parsec.ByteString (Parser)


type Attribute = (String, String) -- (Type, Value)
parseAttribute :: Parser Attribute
parseAttribute = (,) <$> p1 <* spaces <* char '=' <* spaces <*> (p2 <|> p1)
  where
    p1 = many1 alphaNum
    p2 = between (char '"') (char '"') (many $ noneOf ['"'])

type Attributes = [Attribute]
parseAttributes :: Parser Attributes
parseAttributes = between (char '[') (char ']') (p1 `sepBy` char ',')
  where p1 = spaces *> parseAttribute <* spaces

type Node = String
data Command = N Node Attributes | E Node Node Attributes
parseCommand :: Parser Command
parseCommand = do
  node <- spaces *> pNode
  do
    _ <- try (spaces *> string "->")
    E node <$> (spaces *> pNode) <*> pAttrs <* pEnd
    <|>
      N node <$> pAttrs <* pEnd
  where
    pNode = many1 alphaNum
    pAttrs = try (spaces *> parseAttributes) <|> pure []
    pEnd = try (spaces *> char ';') <|> space

parseCommands :: Parser [Either Attribute Command]
parseCommands = many pC
  where pC = (Right <$> try parseCommand <|> Left <$> parseAttribute) <* spaces

type Commands = [Command]
parseDot :: Parser (Attributes, Commands)
parseDot = (lefts &&& rights) <$> (string "digraph" *> spaces *> pName *> spaces *> between (char '{' <* spaces) (char '}') parseCommands)
  where pName = many alphaNum
