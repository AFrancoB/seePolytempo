module Parser(polytemporal, check, test) where

import Prelude

import Data.Identity
import Data.List (List(..), head, tail, elem, (:), filter)
import Data.Array (fromFoldable) as A
import Data.Either
import Data.Int
import Data.Tuple (Tuple(..), fst, snd)
import Data.Map (Map(..), lookup, keys, singleton, fromFoldable, toUnfoldable, member)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set

import Data.FunctorWithIndex (mapWithIndex)

import Data.String.CodeUnits (fromCharArray)

import Parsing
import Parsing.String
import Parsing.String.Basic
import Parsing.Combinators
import Parsing.Combinators.Array (many)
import Parsing.Language (haskellStyle)
import Parsing.Token (makeTokenParser)

import AST
import Rhythm

type P = ParserT String Identity

-- temporal not polytemporal
polytemporal:: P (Map String Temporal)
polytemporal = do
    whitespace
    -- x <- polytemporalRelation `endBy` charWS ';'
    x <- many polytemporalRelation
    _ <- pure 1
    eof
    pure $ fromFoldable x

polytemporalRelation:: P (Tuple String Temporal)
polytemporalRelation = do
    _ <- pure 1
    whitespace
    x <- choice [ try kairos, try metric, converge]
    _ <- charWS '|'
    y <- rhythmic
    pure $ Tuple (fst x) $ Temporal (snd x) (fst y) (snd y)

kairos:: P (Tuple String Polytemporal)
kairos = do
    _ <- pure 1
    id <- voiceId
    _ <- reserved "<-"
    n <- choice [toNumber' <$> naturalOrFloat,asap]
    t <- tempo <|> pure 120.0 -- the alternative should be same as estuary tempo
    pure $ Tuple id $ Kairos n t

asap:: P Number
asap = do
    _ <- pure 1
    x <- charWS '_'
    pure 0.1 -- this has to be adjusted later

metric:: P (Tuple String Polytemporal)
metric = do
    _ <- pure 1
    id <- voiceId
    _ <- reserved "<-"
    x <- choice [toNumber' <$> naturalOrFloat, cTo]
    y <- choice [toNumber' <$> naturalOrFloat, cFrom]
    t <- tempo <|> pure 120.0 -- the alternative should be same as estuary tempo
    pure $ Tuple id $ Metric x y t

cTo:: P Number 
cTo = do
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
    _ <- reserved "<-"
    _ <- whitespace
    voice <- voiceId
    x <- choice [toNumber' <$> naturalOrFloat, cTo]
    y <- choice [toNumber' <$> naturalOrFloat, cFrom]
    t <- tempo <|> pure 120.0 -- the alternative should be same as estuary tempo
    pure $ Tuple id $ Converge voice x y t

voiceId:: P String 
voiceId = do
    _ <- pure 1
    x <- identifier -- many $ noneOf ['\\','<',' ']
    pure x

tempo:: P Number 
tempo = do
  _ <- pure 1
  x <- toNumber' <$> naturalOrFloat
  _ <- reserved "bpm"
  pure x

cpMark:: P Index
cpMark = do
  _ <- pure 1
  x <- parens cpMark'
  pure x

cpMark':: P Index
cpMark' = do
  _ <- pure 1
  v <- natural
  _ <- reserved "-"
  st <- structParser
  e <- parens natural
  pure $ Index v st e

structParser:: P (Array Int)
structParser = do
  _ <- pure 1
  xs <- natural `sepBy` string "."
  pure $ A.fromFoldable xs


test :: String -> Either String (Map String Temporal)
test x =
  case runParser x polytemporal of
    Left (ParseError err _) -> Left err
    Right aMap -> case check aMap of
                    true -> Right aMap
                    false -> Left "failed the check"

check :: Map String Temporal -> Boolean
check aMap = not $ elem false $ mapWithIndex (check2 aMap Nil) aMap   

check2 :: Map String Temporal -> List String -> String -> Temporal -> Boolean
check2 aMap alreadyRefd aKey (Temporal (Kairos _ _) _ _) = true
check2 aMap alreadyRefd aKey (Temporal (Metric _ _ _) _ _) = true
check2 aMap alreadyRefd aKey (Temporal (Converge anotherKey _ _ _) _ _) =
  case lookup anotherKey aMap of
    Nothing -> false
    Just anotherValue -> case elem aKey alreadyRefd of
                           true -> false
                           false -> check2 aMap (aKey : alreadyRefd) anotherKey anotherValue

tokenParser = makeTokenParser haskellStyle
parens      = tokenParser.parens
braces      = tokenParser.braces
identifier  = tokenParser.identifier
reserved    = tokenParser.reserved
naturalOrFloat = tokenParser.naturalOrFloat
natural = tokenParser.natural
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