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

import Parsing
import Parsing.String
import Parsing.String.Basic
import Parsing.Combinators
import Parsing.Combinators.Array (many)
import Parsing.Language (haskellStyle)
import Parsing.Token (makeTokenParser)

import Parser

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
      program: "v0 <- 120bpm _ | x :|" 
  }

render state =
  HH.div
    [ HP.id "root" ]
    [ header
    , subHeader
    , abstract
    , program "program" "\\v0 <- 120bpm _ | x :|",
    HH.text $ f $ pErrorToString $ runParser state.program polytemporal
   ]

f:: Either String (Map String Temporal) -> String
f (Left x) = x
f (Right x) = show x

pErrorToString:: Either ParseError (Map String Temporal) -> Either String (Map String Temporal)
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

