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


-- import SvgHalogen
-- this ones are used for try.purescript.org
import Data.Bifunctor (lmap)
import Data.Array (fromFoldable)
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
  Tempi (Array (Maybe Number)) |
  V1 (Maybe Number) | 
  V2 (Maybe Number) |
  Eval (Maybe Number) |
  CP (Maybe Number) |
  -- ConvergeAt (Maybe Number) | 
  ConvergingFrom (Maybe Number) |
  WS (Maybe Number) |
  WE (Maybe Number)

-- the type of Tempi is wrong, watch out!!
type State = {
    tempi:: Array (Maybe Number), 
    v1:: Maybe Number, 
    v2:: Maybe Number, 
    eval:: Maybe Number,
    cP:: Maybe Number,
    cFrom:: Maybe Number, 
    ws:: Maybe Number,
    we:: Maybe Number
    }


component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ = {
      tempi: [Just 120.0, Just 90.0], 
      v1: Just 10.0, 
      v2: Just 13.0, 
      eval: Just 5.0,
      cP: Just 5.0,
      cFrom: Just 11.0,
      ws: Just 0.0,
      we: Just 20.0
  }

render state =
  HH.div
    [ HP.id "root" ]
    [ header
    , subHeader
    , abstract
    , tempi "tempi" "120,90"
    , voice1 "Number of isochronous events of voice to be converged" "10"
    -- , convergeAt "converged at" "7"
    , eval "evaluation at" "5"
    , convergencePoint "convergence point at" "5"
    , voice2 "Number of isochronous events of voice converging" "13"
    , convergingFrom "converging from" "11"
    , ws "window start (in seconds)" "0.0"
    , we "window end (in seconds)" "20.0"
    -- , eval "evaluate"
    -- , HH.text $ calculationExplained (fromMaybe (Just 2666.0) $ state.tempi!!0) (fromMaybe (Just 2666.0) $ state.tempi!!1) state.v1 state.v2 state.cAt state.cFrom
--    , HH.div_ [HH.text $ calculateWindow state 5.0 5.0]
    , HH.div [ HP.style "left: 2000px;" ] $ [svgNodeToHtml $ svg state]
    ]

svg st = SvgElement { 
    name: "svg"
  , attributes: Li.fromFoldable [ 
      SvgAttribute "xmlns" "http://www.w3.org/2000/svg"
    , SvgAttribute  "viewBox" (show ws <>" 0 " <> show we <> " 10")
    , SvgAttribute "preserveAspectRatio" "xMidYMin meet"
    , SvgAttribute "height" "250"
    , SvgAttribute "width" "600"
    ]
  , children: Li.fromFoldable $ concat [
        [drawCP st ws we]
      , drawlines st ws we
      , drawgrids st ws we
      , drawEvents st ws we
      ]
  }
    where ws = f st.ws
          we = f st.we
  
circle n = SvgElement {name: "circle"
  , attributes: Li.fromFoldable [ 
      SvgAttribute "cx" $ toString n
    , SvgAttribute "cy" "50"
    , SvgAttribute "r" "5"
    , SvgAttribute "stroke" "black"]
  , children: Li.fromFoldable []
}

calculationExplained:: Maybe Number -> Maybe Number -> Maybe Number -> Maybe Number -> Maybe Number -> Maybe Number -> String
calculationExplained t1 t2 v1 v2 cAt cFrom = 
  "You want a voice with a tempo of " <> showMaybeNum t1 <>
  " and " <> showMaybeNum v1 <> " number of events to converge at event index " <> showMaybeNum cAt <>
  " against a voice with a tempo of " <>showMaybeNum  t2 <>
  " and " <> showMaybeNum v2 <> " number of events to converge from event index " <> showMaybeNum cFrom <>
  ", right?"

showMaybeNum:: Maybe Number -> String
showMaybeNum (Just x) = show x
showMaybeNum Nothing = "?? :/ ??"

header :: forall w i. HH.HTML w i
header = HH.h1_ [ HH.text "timekNot - calculating canonic relationships" ]
  
subHeader:: forall w i. HH.HTML w i
subHeader = HH.h2_ [ HH.text "A widget to visualise the possibilities of the language timekNot"]

abstract:: forall w i. HH.HTML w i
abstract = HH.div_ [ HH.text abs ]
  
abs:: String 
abs = "In this website you will find a simple tool to visualise polytemporality"

-- input:: forall w i. String -> String -> HH.HTML w i
tempi d i = HH.div_ [ 
    HH.div_ [HH.text d]
  , HH.input [ HP.type_ HP.InputText
              , HP.placeholder i
              , HE.onValueInput \str -> Tempi $ map fromString $ split (Pattern ",") str
              -- , HP.value state.input
              ]
  ]

voice1 d i = HH.div_ [ 
    HH.div_ [HH.text d]
  , HH.input [ HP.type_ HP.InputText
              , HP.placeholder i
              , HE.onValueInput \str -> V1 (fromString str)
              -- , HP.value state.input
              ]
  ]

voice2 d i = HH.div_ [ 
    HH.div_ [HH.text d]
  , HH.input [ HP.type_ HP.InputText
              , HP.placeholder i
              , HE.onValueInput \str -> V2 (fromString str)
              -- , HP.value state.input
              ]
  ]

eval d i = HH.div_ [ 
    HH.div_ [HH.text d]
  , HH.input [ HP.type_ HP.InputText
              , HP.placeholder i
              , HE.onValueInput \str -> Eval $ fromString str
              -- , HP.value state.input
              ]
  ]

convergencePoint d i = HH.div_ [ 
    HH.div_ [HH.text d]
  , HH.input [ HP.type_ HP.InputText
              , HP.placeholder i
              , HE.onValueInput \str -> CP $ fromString str
              -- , HP.value state.input
              ]
  ]
convergingFrom d i = HH.div_ [ 
    HH.div_ [HH.text d]
  , HH.input [ HP.type_ HP.InputText
              , HP.placeholder i
              , HE.onValueInput \str -> ConvergingFrom $ fromString str
              -- , HP.value state.input
              ]
  ]

ws d i = HH.div_ [ 
    HH.div_ [HH.text d]
  , HH.input [ HP.type_ HP.InputText
              , HP.placeholder i
              , HE.onValueInput \str -> WS $ fromString str
              -- , HP.value state.input
              ]
  ]

we d i = HH.div_ [ 
    HH.div_ [HH.text d]
  , HH.input [ HP.type_ HP.InputText
              , HP.placeholder i
              , HE.onValueInput \str -> WE $ fromString str
              -- , HP.value state.input
              ]
  ]

-- button:: forall w i. String -> HH.HTML w i
button tx = HH.button [ ] [HH.text tx]
-- button tx = HH.button [ HE.onClick \_ -> Clicked ] [HH.text tx]

    -- HH.div_
    --   [ HH.button [ HE.onClick \_ -> Decrement ] [ HH.text "-" ]
    --   , HH.div_ [ HH.text $ show state ]
    --   , HH.button [ HE.onClick \_ -> Increment ] [ HH.text "+" ],
    --     HH.div_ [HH.text "hola"]
    --   ]

setTempi :: Array (Maybe Number) -> State -> State
setTempi ns state = state { tempi = ns }

setVoice1 :: Maybe Number -> State -> State
setVoice1 n state = state { v1 = n }

setVoice2 :: Maybe Number -> State -> State
setVoice2 n state = state { v2 = n }

setEval :: Maybe Number -> State -> State
setEval e state = state { eval = e }

setCP :: Maybe Number -> State -> State
setCP e state = state { cP = e }

setConvergingFrom :: Maybe Number  -> State -> State
setConvergingFrom n state = state { cFrom = n }

setWS :: Maybe Number -> State -> State
setWS n state = state { ws = n }

setWE :: Maybe Number -> State -> State
setWE n state = state { we = n }

-- state = {tempi: [Just 120.0, Just 90.0], v1: Just 10.0, v2: Just 13.0, cAt: Just 7.0, cFrom: Just 11.0}
-- state = {tempi: "120,90", v1: "10", cAt: "7", v2: "13", cTo: "11"}

handleAction :: forall output m. Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Tempi mns -> H.modify_ \state -> setTempi mns state
  V1 mn -> H.modify_ \state -> setVoice1 mn state
  V2 mn -> H.modify_ \state -> setVoice2 mn state
  Eval mn -> H.modify_ \state -> setEval mn state
  CP mn -> H.modify_ \state -> setCP mn state
  ConvergingFrom mn -> H.modify_ \state -> setConvergingFrom mn state
  WS mn -> H.modify_ \state -> setWS mn state
  WE mn -> H.modify_ \state -> setWE mn state


drawCP:: State -> Number -> Number -> SvgNode
drawCP st ws we = 
  let cp = (f st.eval + (f st.cP)) / f st.v1
      dur = durInSecs $ Tuple (f st.v1) $ f (fromMaybe (Just 1.0) (st.tempi!!0))
  in cpLine $ dur * cp

cpLine xs = SvgElement {name: "line"
  , attributes: Li.fromFoldable [
      SvgAttribute "x1" $ toString xs
    , SvgAttribute "x2" $ toString xs
    , SvgAttribute "y1" $ toString 6.0
    , SvgAttribute "y2" $ toString 4.0
    , SvgAttribute "stroke" "red"
    , SvgAttribute "stroke-width" "0.25"
    , SvgAttribute "opacity" "0.75"
  ]
  , children: Li.fromFoldable []
}

drawEvents:: State -> Number -> Number -> Array SvgNode
drawEvents st ws we =
  let window = calculateEvents st ws we
      eventIndex1 = fst $ eventIndex st ws we
      (Tuple v1Start v1End) = Tuple (fst window.start) (fst window.end)
      (Tuple v2Start v2End) = Tuple (snd window.start) (snd window.end) 
      widthsV1 = lengthsOfCanonic (durInSecs $ Tuple (1.0) (f $ fromMaybe (Just 1.0) (st.tempi!!0))) v1Start v1End
      widthsV2 = lengthsOfCanonic (durInSecs $ Tuple (1.0) (f $ fromMaybe (Just 1.0) (st.tempi!!1))) v2Start v2End
      x1sV1 = fromMaybe [0.0] $ init $ ws :(scanl (+) ws widthsV1)
      x1sV2 = fromMaybe [0.0] $ init $ ws :(scanl (+) ws widthsV2)
      line1 = map (\x -> line'' 2.0 1.0 (fst x) $ snd x) $ zip widthsV1 x1sV1
      line2 = map (\x -> line'' 8.0 1.0 (fst x) $ snd x) $ zip widthsV2 x1sV2
  in concat [line1, line2]

label ys (Tuple x tx) = SvgElement {
  name: "text",
  attributes: Li.fromFoldable [
      SvgAttribute "x" $ toString x
    , SvgAttribute "y" $ toString ys
    , SvgAttribute "font-size" "0.75px"
    ],
  children: Li.fromFoldable [SvgText $ toString $ toNumber tx]
}
  
drawgrids:: State -> Number -> Number -> Array SvgNode
drawgrids st ws we = 
  let window = calculateEvents st ws we
      (Tuple v1Start v1End) = Tuple (fst window.start) (fst window.end)
      (Tuple v2Start v2End) = Tuple (snd window.start) (snd window.end) 
      widthsV1 = lengthsOfCanonic (durInSecs $ Tuple (1.0) (f $ fromMaybe (Just 1.0) (st.tempi!!0))) v1Start v1End
      widthsV2 = lengthsOfCanonic (durInSecs $ Tuple (1.0) (f $ fromMaybe (Just 1.0) (st.tempi!!1))) v2Start v2End
      x1sV1 = fromMaybe [0.0] $ init $ ws :(scanl (+) ws widthsV1)
      x1sV2 = fromMaybe [0.0] $ init $ ws :(scanl (+) ws widthsV2)
      line1 = map (\x -> line' 3.5 3.0 (fst x) $ snd x) $ zip widthsV1 x1sV1
      line2 = map (\x -> line' 6.5 3.0 (fst x) $ snd x) $ zip widthsV2 x1sV2
  in concat [line1,line2]

line' ys w width x1 = SvgElement {name: "line"
  , attributes: Li.fromFoldable [ 
      SvgAttribute "x1" $ toString x1
    , SvgAttribute "x2" $ toString (x1 + 0.05)
    , SvgAttribute "y1" $ toString ys
    , SvgAttribute "y2" $ toString ys
    , SvgAttribute "stroke" "black"
    , SvgAttribute "stroke-width" $ toString w
    , SvgAttribute "opacity" "0.25"]
  , children: Li.fromFoldable []
}

drawlines:: State -> Number -> Number -> Array SvgNode
drawlines st ws we = 
  let window = calculateWindow st ws we
      (Tuple v1Start v1End) = Tuple (fst window.start) (fst window.end)
      (Tuple v2Start v2End) = Tuple (snd window.start) (snd window.end) 
      widthsV1 = lengthsOfCanonic (durInSecs $ Tuple (f st.v1) (f $ fromMaybe (Just 1.0) (st.tempi!!0))) v1Start v1End
      widthsV2 = lengthsOfCanonic (durInSecs $ Tuple (f st.v2) (f $ fromMaybe (Just 1.0) (st.tempi!!1))) v2Start v2End
      x1sV1 = fromMaybe [0.0] $ init $ ws :(scanl (+) ws widthsV1)
      x1sV2 = fromMaybe [0.0] $ init $ ws :(scanl (+) ws widthsV2)
      line1 = map (\x -> line 4.0 1.0 (fst x) $ snd x) $ zip widthsV1 x1sV1
      line2 = map (\x -> line 6.0 1.0 (fst x) $ snd x) $ zip widthsV2 x1sV2
  in concat [line1,line2]

line ys w width x1 = SvgElement {name: "line"
  , attributes: Li.fromFoldable [ 
      SvgAttribute "x1" $ toString x1
    , SvgAttribute "x2" $ toString $ (x1 + width)-0.05
    , SvgAttribute "y1" $ toString ys
    , SvgAttribute "y2" $ toString ys
    , SvgAttribute "stroke" "black"
    , SvgAttribute "stroke-width" $ toString w
    , SvgAttribute "opacity" "0.75"]
  , children: Li.fromFoldable []
}

line'' ys w width x1 = SvgElement {name: "line"
  , attributes: Li.fromFoldable [ 
      SvgAttribute "x1" $ toString x1
    , SvgAttribute "x2" $ toString $ (x1 + width)-0.05
    , SvgAttribute "y1" $ toString ys
    , SvgAttribute "y2" $ toString ys
    , SvgAttribute "stroke" "black"
    , SvgAttribute "stroke-width" $ toString w
    , SvgAttribute "opacity" "0.35"]
  , children: Li.fromFoldable []
}

  
-- still not good
eventIndex:: State -> Number -> Number -> Tuple (Array Int) (Array Int)
eventIndex st ws we =
  let window = calculateWindow st ws we
      (Tuple v1Start v1End) = Tuple ((fst window.start)* f st.v1) $ fst window.end * f st.v1
      (Tuple v2Start v2End) = Tuple ((snd window.start)* f st.v2) $ snd window.end * f st.v2
      v1 = ceil v1Start .. floor v1End
      v2 = ceil v2Start .. floor v2End
  in Tuple v1 v2

-- inferConvergeAtAndConvergingFromBasedOnEval:: State -> Number -> Number -> State
-- inferConvergeAtAndConvergingFromBasedOnEval st eval projectedCP =
 -- let evalWitgProjectedCP = eval + projectedCP
      -- v1 = durInSecs $ Tuple (st tempo1) (st v1)
      -- do the same with tempo, voice2
      -- from there who knows....


lengthsOfCanonic:: Number -> Number -> Number -> Array Number
lengthsOfCanonic durInSecs start end
  | (floor start) == (floor end) = map (\x -> durInSecs*x) [end - start]
  | (floor start) == ((floor end) - 1) = map (\x -> x*durInSecs) [ceiled-start, end-floored]
    where ceiled = (toNumber $ ceil start)
          floored = (toNumber $ floor end) 
  | otherwise = 
    let ceiled = (toNumber $ ceil start)
        floored = (toNumber $ floor end)
        first = ceiled - start
        last = end - floored
        amountOfCompletes = replicate (floor end - ceil start) 1.0
    in map (\x -> x*durInSecs) $ concat [[first],amountOfCompletes,[last]]

type Window = {start:: Tuple Number Number, end:: Tuple Number Number}

calculateEvents:: State -> Number -> Number -> Window
calculateEvents state ws we = {start: wStart, end: wEnd}
  where voice1 = Tuple (1.0) $ f (fromMaybe (Just 1.0) $ state.tempi!!0)
        voice2 = Tuple (1.0) $ f (fromMaybe (Just 1.0) $ state.tempi!!1)
        cAt = ((f state.eval) + (f state.cP)) / 1.0
        cFrom = f state.cFrom / 1.0
        wStart = converge ws voice1 cAt voice2 cFrom
        wEnd = converge we voice1 cAt voice2 cFrom

-- first tuple: startVoice1 startvoice2  second tuple: endvoice1 endvoice2
calculateWindow:: State -> Number -> Number -> Window
calculateWindow state ws we = {start: wStart, end: wEnd}
  where voice1 = Tuple (f state.v1) $ f (fromMaybe (Just 1.0) $ state.tempi!!0)
        voice2 = Tuple (f state.v2) $ f (fromMaybe (Just 1.0) $ state.tempi!!1)
        cAt = ((f state.eval) + (f state.cP)) / f state.v1
        cFrom = f state.cFrom / f state.v2
        wStart = converge ws voice1 cAt voice2 cFrom
        wEnd = converge we voice1 cAt voice2 cFrom

f:: Maybe Number -> Number
f n = fromMaybe 1.0 n

-- output: Tuple voice1PercentageAtMoment voice2PercentageAtMoment 
converge:: Number -> Tuple Number Number -> Number -> Tuple Number Number -> Number -> Tuple Number Number
converge moment line1 convergedAt line2 convergingFrom = Tuple conv1 (conv2 + (lineToPercentage ((convergingFrom*durInSecs line2) - (convergedAt*durInSecs line1)) line2))
    where conv1 = lineToPercentage moment line1
          conv2 = lineToPercentage moment line2

findPoint:: Number -> Number -> Tuple Number Number -> Number
findPoint moment point line = ((durInSecs line)*point) - moment

lineToPercentage:: Number -> Tuple Number Number -> Number
lineToPercentage moment line = moment / durInSecs line

durInSecs:: Tuple Number Number -> Number
durInSecs line = fst line * (bpmToDur (snd line))

line1' = Tuple 12.0 90.0 -- 13 time untis at 90 bpm

line2' = Tuple 12.0 120.0 -- 17 time untis at 135 bpm

bpmToFreq bpm = (1.0/60.0)* bpm

freqToDur freq = 1.0 / freq

bpmToDur bpm = 1.0 / bpmToFreq bpm

st = {tempi: [Just 120.0, Just 90.0], v1: Just 10.0, v2: Just 13.0, cAt: Just 7.0, cFrom: Just 11.0}


----  is this correct? think about it and then implement




    
data Convergence = Convergence Number Number  -- numbers should be substituted with a proper coordinate system
instance Show Convergence where
    show (Convergence x y) = show x <> " & " <> show y

data Voice = Voice Number Convergence
instance Show Voice where
    show (Voice t conv) = " t: " <> show t <> " conv: " <> show conv

type PolyTempo = M.Map String Voice



---- halogen svg stuff
ns :: HH.Namespace
ns = HH.Namespace "http://www.w3.org/2000/svg"

svgAttributeToProp :: forall r i. SvgAttribute -> HP.IProp r i
svgAttributeToProp (SvgAttribute k v) = HP.attr (HH.AttrName k) v

svgElementToHtml :: forall p i. Element -> HH.HTML p i
svgElementToHtml ele = svgElementToHtmlWithAttrs ele []

svgElementToHtmlWithAttrs :: forall p r i. Element -> Array (HP.IProp r i) -> HH.HTML p i
svgElementToHtmlWithAttrs ele newAttrs =
  HH.elementNS ns (HH.ElemName ele.name) (attrs <> newAttrs) children
  where
    attrs = fromFoldable $ svgAttributeToProp <$> ele.attributes
    children = fromFoldable $ svgNodeToHtml <$> ele.children

svgNodeToHtml :: forall p i. SvgNode -> HH.HTML p i
svgNodeToHtml (SvgElement element) = svgElementToHtml element
svgNodeToHtml (SvgText str) = HH.text str
svgNodeToHtml (SvgComment _str) = HH.text ""
