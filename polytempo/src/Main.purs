module Main where

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
import Data.Number (fromString)
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

import Parser
import Rhythm
import Visualisation

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

data Action =
  Program String |
  WS (Maybe Number) |
  WE (Maybe Number) |
  Eval (Maybe Number)

type State = {
  program:: String,
  ws:: Maybe Number,
  we:: Maybe Number,
  eval:: Maybe Number
  }


component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ = {
      program: "v0 <- 120bpm _ | x :|",
      ws: Just 0.0,
      we: Just 20.0,
      eval: Just 0.0
  }

render state =
  HH.div
    [ HP.id "root" ]
    [ header
    , subHeader
    , abstract
    , ws "window start: " "0.0"
    , we "window end: " "20.0"
    , eval "evaluation time:" "0.0"
    , program "program" "\\v0 <- 120bpm _ | x :|"
    , check' (f state.ws) (f state.we) (f state.eval) state.program
   ]


f:: Maybe Number -> Number
f n = fromMaybe 2.666 n

-- make a error comment for the check
-- check :: Map String Temporal -> Boolean     -- Either String (Map String Temporal)
-- check':: Number -> Number -> Number -> Either String (Map String Temporal) -> HH.HTML w i 
-- check' ws we eval (Left x) = x
-- check' ws we eval (Right x) = if check x then drawProgram x ws we eval else HH.text $ "fail check: " <> show x

check' ws we eval program =
  case runParser program polytemporal of
    Left (ParseError err _) -> HH.text err
    Right aMap -> case check aMap of
                    true -> drawProgram aMap ws we eval
                    false -> HH.text "failed the check"


pErrorToString:: Either ParseError (Map String Temporal) -> Either String (Map String Temporal)
pErrorToString (Left x) = Left $ parseErrorMessage x
pErrorToString (Right x) = Right x


header :: forall w i. HH.HTML w i
header = HH.h1_ [ HH.text "timekNot - a language for polytemporal relationships" ]
  
subHeader:: forall w i. HH.HTML w i
subHeader = HH.h2_ [ HH.text "A widget to visualise the possibilities of the language timekNot"]

abstract:: forall w i. HH.HTML w i
abstract = HH.div_ [ HH.text abs ]
  
abs:: String 
abs = "In this website you will find a simple tool to visualise polytemporality. Just enter a timekNot program in the text box below"

ws d i = HH.div_ [ 
    HH.div_ [HH.text d]
  , HH.input [ HP.type_ HP.InputText
              , HP.placeholder i
              , HE.onValueInput \str -> WS (fromString str)
              -- , HP.value state.input
              ]
  ]

we d i = HH.div_ [ 
    HH.div_ [HH.text d]
  , HH.input [ HP.type_ HP.InputText
              , HP.placeholder i
              , HE.onValueInput \str -> WE (fromString str)
              -- , HP.value state.input
              ]
  ]

eval d i = HH.div_ [ 
    HH.div_ [HH.text d]
  , HH.input [ HP.type_ HP.InputText
              , HP.placeholder i
              , HE.onValueInput \str -> Eval (fromString str)
              -- , HP.value state.input
              ]
  ]

-- input:: forall w i. String -> String -> HH.HTML w i
program d i = HH.div_ [ 
      HH.div_ [HH.text d]
    , HH.textarea
        [ 
          -- HP.class_ "edit"
        -- , HP.value state.program
          HP.placeholder i
        , HE.onValueInput \s -> Program s
        ]
  -- , HH.input [ HP.type_ HP.InputText
  --             , HP.placeholder i
  --             , HE.onValueInput \str -> Program str
  --             -- , HP.value state.input
  --             ]
  ]

setProgram:: String -> State -> State
setProgram ns state = state { program = ns }

setWS::Maybe Number -> State -> State
setWS mn state = state { ws = mn }

setWE::Maybe Number -> State -> State
setWE mn state = state { we = mn }

setEval:: Maybe Number -> State -> State
setEval mn state = state { eval = mn } 

handleAction :: forall output m. Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Program str -> H.modify_ \state -> setProgram str state
  WS mn -> H.modify_ \state -> setWS mn state
  WE mn -> H.modify_ \state -> setWE mn state
  Eval mn -> H.modify_ \state -> setEval mn state

