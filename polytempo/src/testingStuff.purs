module Trial where

import Prelude

import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

import Svg.Parser

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


-- import SvgHalogen
-- this ones are used for try.purescript.org
import Data.Bifunctor (lmap)
-- import Data.Array (fromFoldable)
import Data.Either (Either(..))

import Data.Number.Format (toString)

import Data.Maybe
import Data.Number
import Data.String
import Data.Array ((!!), replicate, concat, (..), (:), init, tail, zip)
import Data.Tuple
import Data.Map as M
import Data.List (fromFoldable) as Li
import Data.Int (ceil, floor, toNumber)
import Data.Traversable (scanl)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

data Action =
  Program String

type State = {program:: String}

component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ = {
      program: "v0 <- _" 
  }

render state =
  HH.div
    [ HP.id "root" ]
    [ header
    , subHeader
    , abstract
    , program "program" "\\v0 <- _ | x :||",
    HH.text $ f $ pErrorToString $ runParser state.program polytemporal
   ]

f:: Either String (Map String Polytemporal) -> String
f (Left x) = x
f (Right x) = show x

pErrorToString:: Either ParseError (Map String Polytemporal) -> Either String (Map String Polytemporal)
pErrorToString (Left x) = Left $ parseErrorMessage x
pErrorToString (Right x) = Right x


header :: forall w i. HH.HTML w i
header = HH.h1_ [ HH.text "timekNot - calculating canonic relationships" ]
  
subHeader:: forall w i. HH.HTML w i
subHeader = HH.h2_ [ HH.text "A widget to visualise the possibilities of the language timekNot"]

abstract:: forall w i. HH.HTML w i
abstract = HH.div_ [ HH.text abs ]
  
abs:: String 
abs = "In this website you will find a simple tool to visualise polytemporality. Just enter a timekNot program in the text box below"

-- input:: forall w i. String -> String -> HH.HTML w i
program d i = HH.div_ [ 
    HH.div_ [HH.text d]
  , HH.input [ HP.type_ HP.InputText
              , HP.placeholder i
              , HE.onValueInput \str -> Program str
              -- , HP.value state.input
              ]
  ]


setProgram :: String -> State -> State
setProgram ns state = state { program = ns }


handleAction :: forall output m. Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Program str -> H.modify_ \state -> setProgram str state






-------

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




