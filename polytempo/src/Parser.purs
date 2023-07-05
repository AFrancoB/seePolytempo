module Parser where

import Prelude

import Data.Identity
import Data.List (List(..), head, tail, elem, (:), filter)
import Data.Either
import Data.Int
import Data.Tuple (Tuple(..), fst, snd)
import Data.Map (Map(..), lookup, keys, singleton, fromFoldable, toUnfoldable, member)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set

import Data.String.CodeUnits (fromCharArray)

import Parsing
import Parsing.String
import Parsing.String.Basic
import Parsing.Combinators
import Parsing.Combinators.Array (many)
import Parsing.Language (haskellStyle)
import Parsing.Token (makeTokenParser)

type P = ParserT String Identity


polytemporal:: P (Map String Polytemporal)
polytemporal = do
    whitespace
    x <- polytemporalRelation `endBy` charWS ';'
    _ <- pure 1
    eof
    pure $ fromFoldable x

polytemporalRelation:: P (Tuple String Polytemporal)
polytemporalRelation = do
    _ <- pure 1
    _ <- reserved ">"
    x <- choice [try metric, kairos, converge]
    pure x

kairos:: P (Tuple String Polytemporal)
kairos = do
    _ <- pure 1
    id <- voiceId
    n <- choice [toNumber' <$> naturalOrFloat,asap]
    pure $ Tuple id $ Kairos n

asap:: P Number
asap = do
    _ <- pure 1
    x <- charWS '_'
    pure 0.0

metric:: P (Tuple String Polytemporal)
metric = do
    _ <- pure 1
    id <- voiceId
    x <- choice [toNumber' <$> naturalOrFloat, cAt]
    y <- choice [toNumber' <$> naturalOrFloat, cFrom]
    pure $ Tuple id $ Metric x y

cAt:: P Number 
cAt = do
    _ <- pure 1
    x <- strWS "_ "
    pure 0.0

cFrom:: P Number 
cFrom = do
    _ <- pure 1
    x <- charWS '_'
    pure 0.0

converge:: P (Tuple String Polytemporal)
converge = do
    _ <- pure 1
    id <- voiceId
    _ <- charWS '\\'
    voice <- many $ noneOf ['\\','<',' ']
    _ <- charWS ' '
    x <- choice [toNumber' <$> naturalOrFloat, cAt]
    y <- choice [toNumber' <$> naturalOrFloat, cFrom]
    pure $ Tuple id $ Converge (fromCharArray voice) x y

voiceId:: P String 
voiceId = do
    _ <- char '\\'
    x <- many $ noneOf ['\\','<',' ']
    _ <- whitespace
    _ <- reserved "<-"
    pure $ fromCharArray x










check':: Map String Polytemporal -> Bool
check' mapa = check checked next mapa
    where checked = (Nil)
          next = fromMaybe "" $ head $ Set.toUnfoldable $ keys mapa

check:: List String -> String -> Map String Polytemporal -> Boolean
check checked next mapa 
    | notConvergent next mapa = true
    | otherwise = if nextChecked then false else check newChecked newNext mapa
    where nextChecked = isNextChecked checked next
          newChecked = (next : checked)
          newNext = fromMaybe "" $ head $ filter (\x -> not $ elem x newChecked) $ Set.toUnfoldable $ keys mapa -- :: List String

notConvergent:: String -> Map String Polytemporal -> Boolean
notConvergent next mapa = f m
    where m = lookup next mapa 

f:: Maybe Polytemporal -> Boolean
f Nothing = false
f (Just x) = g x 

g (Kairos _) = true 
g (Metric _ _) = true
g (Converge _ _ _) = false

isNextChecked:: List String -> String -> Boolean
isNextChecked checked next = elem next checked



-- check :: List String -> String -> Map String Layer -> Bool

-- check thingsAlreadyRefd nextReference map

-- check ["x"] "x" map ...
-- if nextReference is universal time, return true
-- otherwise, if the next layer to be referenced is already in the list, return false
-- if the next layer is not already in the list, add it to the list and return the recursive result of check theList theNextRef theMap



data Polytemporal = 
  Kairos Number | -- Arg: universal time unit (miliseconds and datetime in purs)
  -- Kairos starts a program at evaluation time (or as soon as possible), no underlying grid
  Metric Number Number | -- 
  Converge String Number Number -- Args: String is the voice identifier, convergAt (where this voice converges with the identified voice) and convergedFrom (the point of this voice that converges with the identified voice)
  -- Converge starts a program in relationship with another voice

instance polytemporalShowInstance :: Show Polytemporal where
  show (Kairos timemark) = "kairos: " <> show timemark
  show (Metric cAt cFrom) = "voice aligns with metric at "<>show cAt<>" from "<>show cFrom
  show (Converge voice cAt cFrom) = "voice aligns with "<>show voice<>" at "<>show cAt<>" from "<>show cFrom


tokenParser = makeTokenParser haskellStyle
parens      = tokenParser.parens
braces      = tokenParser.braces
identifier  = tokenParser.identifier
reserved    = tokenParser.reserved
naturalOrFloat = tokenParser.naturalOrFloat
float = tokenParser.float
whitespace = tokenParser.whiteSpace
colon = tokenParser.colon
brackets = tokenParser.brackets
comma = tokenParser.comma
semi = tokenParser.semi
integer = tokenParser.integer
stringLiteral = tokenParser.stringLiteral
reservedOp = tokenParser.reservedOp


toNumber':: Either Int Number -> Number
toNumber' (Left x) = toNumber x 
toNumber' (Right x) = x


charWS:: Char -> P Char
charWS x = do
  _ <- pure 1
  x <- char x 
  whitespace
  pure x

strWS:: String -> P String
strWS x = do
  _ <- pure 1
  x <- string x 
  whitespace
  pure x