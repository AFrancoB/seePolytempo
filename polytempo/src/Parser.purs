module Parser(polytemporal, Temporal(..), Polytemporal(..)) where

import Prelude

import Data.Identity
import Data.List (List(..), head, tail, elem, (:), filter)
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
    x <- choice [try metric, try kairos, converge]
    _ <- charWS '|'
    y <- rhythmic
    pure $ Tuple (fst x) $ Temporal (snd x) (fst y) (snd y)

kairos:: P (Tuple String Polytemporal)
kairos = do
    _ <- pure 1
    id <- voiceId
    _ <- reserved "<-"
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
    _ <- reserved "<-"
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
    _ <- reserved "<-"
    _ <- whitespace
    voice <- voiceId
    x <- choice [toNumber' <$> naturalOrFloat, cAt]
    y <- choice [toNumber' <$> naturalOrFloat, cFrom]
    pure $ Tuple id $ Converge voice x y

voiceId:: P String 
voiceId = do
    _ <- pure 1
    _ <- char '\\'
    x <- many $ noneOf ['\\','<',' ']
    _ <- charWS ' '
    pure $ fromCharArray x



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
check2 aMap alreadyRefd aKey (Temporal (Kairos _) _ _) = true
check2 aMap alreadyRefd aKey (Temporal (Metric _ _) _ _) = true
check2 aMap alreadyRefd aKey (Temporal (Converge anotherKey _ _) _ _) =
  case lookup anotherKey aMap of
    Nothing -> false
    Just anotherValue -> case elem aKey alreadyRefd of
                           true -> false
                           false -> check2 aMap (aKey : alreadyRefd) anotherKey anotherValue


data Temporal = Temporal Polytemporal Rhythmic Boolean

instance temporalShow :: Show Temporal where
    show (Temporal x y z) = show x <> " " <> show y <> (if z then "looped" else "unlooped")


data Polytemporal = 
  Kairos Number | -- Arg: universal time unit (miliseconds and datetime in purs)
  -- Kairos starts a program at evaluation time (or as soon as possible), no underlying grid
  Metric Number Number | -- starts a program attached to a default underlying voice (a tempo grid basically) first number is the point to where the new voice will converge, second number is the point from which it converges. first _ is 0 and second _ is 0 (so both voices align at index 0)
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