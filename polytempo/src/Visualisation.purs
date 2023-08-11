module Visualisation (durFromRhythmic,calculateSVGElements, funca, test1, test2, test3, test4, test5, test6,test7, test8, drawProgram, lineToPercentageInSecs, findBeats, converge, addElapsing, testy, funquilla, Triplet(..)) where

import Prelude

import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)


import Data.Tuple
import Data.Maybe
import Data.Map as M
import Data.Foldable (sum)
import Data.Int
import Data.FunctorWithIndex
import Data.Array (filter,fromFoldable,(!!), replicate, concat, (..), (:), init, tail, last,head,reverse)
import Data.List
import Data.Number.Format (toString)
import Data.Traversable (scanl)
import Data.List (fromFoldable,concat) as Li
import Data.Ord (signum)

import Svg.Parser

import Parsing
import Parser
import Rhythm


-- drawProgram:: M.Map String Temporal -> Number -> Number -> Number -> HH.HTML p i
drawProgram mapa ws we eval = svgNodeToHtml $ svgFrame ws we h $ calculateVoiceLines $ funca mapa ws we eval
  where h = toNumber $ length $ M.values mapa
------svgNodeToHtml:: forall p i. SvgNode -> HH.HTML p i

svgFrame:: Number -> Number -> Number -> Array SvgNode -> SvgNode
svgFrame ws we h nodes = SvgElement { 
    name: "svg"
  , attributes: Li.fromFoldable [ 
      SvgAttribute "xmlns" "http://www.w3.org/2000/svg"
    , SvgAttribute  "viewBox" (show ws <>" -1 " <> show (we - ws) <> " " <> show (h + 1.0))
    , SvgAttribute "preserveAspectRatio" "xMidYMid meet"
    , SvgAttribute "height" "350"
    , SvgAttribute "width" "1000"
    ]
  , children: Li.concat $ Li.fromFoldable [Li.fromFoldable [defs], Li.fromFoldable nodes, Li.fromFoldable [background ws we h]]
      
  }


defs = SvgElement {
    name: "defs"
  , attributes: Li.fromFoldable []
  , children: Li.fromFoldable [grad]
}

grad = SvgElement {
    name: "linearGradient"
  , attributes: Li.fromFoldable [
        SvgAttribute "id" "grad"
      , SvgAttribute "x1" "0%"
      , SvgAttribute "y1" "0%"
      , SvgAttribute "x2" "100%"
      , SvgAttribute "y2" "0%"
  ]
  , children: Li.fromFoldable [stop1,stop2]
}

stop1 = SvgElement {
    name: "stop"
  , attributes: Li.fromFoldable [
      SvgAttribute "offset" "0%"
    , SvgAttribute "style" "stop-color:black; stop-opacity:1"
  ] 
  , children: Li.fromFoldable []
}

stop2 = SvgElement {
    name: "stop"
  , attributes: Li.fromFoldable [
      SvgAttribute "offset" "100%"
    , SvgAttribute "style" "stop-color:transparent; stop-opacity:1"
  ] 
  , children: Li.fromFoldable []
}

background ws we h = SvgElement {
    name: "rect"
  , attributes: Li.fromFoldable [
      SvgAttribute "x" $ show ws
    , SvgAttribute "y" "-0.25"
    , SvgAttribute "width" $ show $ we - ws
    , SvgAttribute "height" $ show $ (h + 1.0) + 0.5
    , SvgAttribute "opacity" "50%"
    , SvgAttribute "fill" "blue"
    ]
  , children: Li.fromFoldable []
}

-- this is for looped events:
-- calculateVoiceDrawing:: Number -> Array Number -> Array SvgNode
-- calculateVoiceDrawing ws widths = map (\x -> drawVoice 4.0 1.0 (fst x) $ snd x) $ zip widths xs
--   where xs = fromMaybe [0.0] $ init $ ws :(scanl (+) ws widths)

calculateVoiceLines:: Array Coordinate -> Array SvgNode
calculateVoiceLines coords = map (\ x -> drawVoiceLines x.x1 x.x2 x.y 0.5) coords

drawVoiceLines:: Number -> Number -> Number -> Number -> SvgNode
drawVoiceLines x1 x2 y wStroke = SvgElement {name: "rect"
  , attributes: Li.fromFoldable [ 
      SvgAttribute "x" $ toString x1
    , SvgAttribute "width" $ toString $ x2 - x1
    , SvgAttribute "y" $ toString y
    , SvgAttribute "height" $ toString 0.1
    , SvgAttribute "fill" "url(#grad)"
    , SvgAttribute "opacity" "0.75"]
  , children: Li.fromFoldable []
}

-- make a funca that substitutes the string index of each layer with a Int index 

funca:: M.Map String Temporal -> Number -> Number -> Number -> Array Coordinate
funca mapa ws we eval = coords
  where calculated = calculateSVGElements mapa ws we eval
        vals = M.values calculated
        len = length vals
        yTups = zip (range 0 len) vals  -- List (Tuple Y (Array (Tuple X1 X2)))
        coords = concat $ fromFoldable $ map (\x -> funca2 x) yTups

-- funca2 :: Tuple Y (Array (Tuple X1 X2))
-- funca2:: 
funca2 item = map (\x -> {x1: fst x, x2: snd x, y: toNumber $ fst item} ) $ snd item


calculateSVGElements:: M.Map String Temporal -> Number -> Number -> Number -> M.Map String (Array (Tuple Number Number))
calculateSVGElements mapa ws we eval = mapWithIndex (calculateSVGElement mapa ws we eval) mapa 


-- make a test
calculateSVGElement:: M.Map String Temporal -> Number -> Number -> Number -> String -> Temporal -> Array (Tuple Number Number) -- Tuple X1 X2
calculateSVGElement mapa ws we eval aKey (Temporal (Kairos asap tempo) rhythmic loop) = if loop then looped else unlooped
  where startOfVoice = eval + asap -- always the start of the program
        dur = durFromRhythmic rhythmic tempo -- dur of voice in seconds
        unlooped = filter (\x -> x /= (Tuple 0.0 0.0)) $ voiceInWindowUnlooped startOfVoice (startOfVoice+dur) ws we
        looped = filter (\x -> x /= (Tuple 0.0 0.0)) $ voiceInWindowLooped startOfVoice (startOfVoice+dur) ws we

calculateSVGElement mapa ws we eval aKey (Temporal (Metric cTo cFrom t) rhythmic loop) = if loop then looped else unlooped  
  where dur = durFromRhythmic rhythmic t
        startOfVoice = calculateStartConvergent defVoiceInSecs cTo dur cFrom 
        unlooped = filter (\x -> x /= (Tuple 0.0 0.0)) $ voiceInWindowUnlooped startOfVoice (startOfVoice+dur) ws we
        looped = filter (\x -> x /= (Tuple 0.0 0.0)) $ voiceInWindowLooped startOfVoice (startOfVoice+dur) ws we

calculateSVGElement mapa ws we eval aKey (Temporal (Converge convergedKey cTo cFrom t) rhythmic loop) = if loop then unlooped else unlooped
  where dur = durFromRhythmic rhythmic t
        (Tuple x1 x2) = convergeFunc mapa convergedKey eval dur cTo cFrom
        unlooped = filter (\x -> x /= (Tuple 0.0 0.0)) $ voiceInWindowUnlooped x1 x2 ws we



testy m converging = convergeFunc m keyConverged eval durConverging cTo cFrom
  where convergingVal = fromMaybe (Temporal (Kairos 0.0 0.0) O false) $ M.lookup converging m  -- debe ser siempre Converge
        eval = 2.0 
        keyConverged = funcaTest convergingVal
        durConverging = funcaTest2 convergingVal
        (Tuple cTo cFrom) =  funcaTest3 convergingVal

funcaTest (Temporal (Converge convergedKey _ _ _) _ _) = convergedKey
funcaTest (Temporal _ _ _) = "meh"

funcaTest2 (Temporal (Converge _ _ _ t) rhy _) = durFromRhythmic rhy t
funcaTest2 (Temporal _ _ _) = 2.666

funcaTest3 (Temporal (Converge _ cTo cFrom _) _ _) = Tuple cTo cFrom
funcaTest3 (Temporal _ _ _) = Tuple 0.666 2.666

-- calculations of convergent relqationships have a bug, tend to be double the width or half the start position in this program:
-- \v0 <- 60bpm 0.0 | xxxx ||
-- \v1 <- 60bpm \v0 0.5 0.0 | xxxx ||
-- v0 = 0.0 to 4.0, v1 = 4.0 to 8.0 which is wrong. it should go from 2.0 to 6.0

-- probably the issue is at convergeFunc, thus funquilla probably is also weird because it uses the same calculations
-- I feel that getting the duration in seconds is being applied too many times to the process.


convergeFunc:: M.Map String Temporal -> String -> Number -> Number -> Number -> Number -> Tuple Number Number
convergeFunc mapa convergedKey eval durInSecs cTo cFrom = Tuple convergingX1 convergingX2
  where cFromInSecs = durInSecs * cFrom 
        convergedValue = fromMaybe (Temporal (Kairos 0.0 0.0) O false) $ M.lookup convergedKey mapa
        (Tuple x1 x2) = findX1AndX2 mapa eval convergedValue (Cons (Triplet durInSecs cTo cFrom) Nil) -- Tuple X1 X2
        cToInSecs = (x2 - x1) * cTo 
        convergencePoint = x1 + cToInSecs 
        convergingX1 = convergencePoint - cFromInSecs
        convergingX2 = convergingX1 + durInSecs

funquilla:: Number -> Number -> List Triplet -> Tuple Number Number
funquilla x1Converged x2Converged (Nil) = Tuple x1Converged x2Converged 
funquilla x1Converged x2Converged (Cons x xs) = 
  let convergedTo = (x2Converged - x1Converged) * (snd3 x)
      convergingFrom = (fst3 x) * (thrd x) 
      convergencePoint = x1Converged + convergedTo
      convergingX1 = convergencePoint - convergingFrom
      convergingX2 = convergingX1 + (fst3 x)
  in funquilla convergingX1 convergingX2 xs

findX1AndX2:: M.Map String Temporal -> Number -> Temporal -> List Triplet -> Tuple Number Number
findX1AndX2 mapa eval (Temporal (Kairos asap t) rhy _) recursive = funquilla x1Converged x2Converged recursive
  where x1Converged = eval + asap -- always the start of the program
        x2Converged = x1Converged + (durFromRhythmic rhy t)

findX1AndX2 mapa eval (Temporal (Metric cTo cFrom t) rhy _) recursive = funquilla x1Converged x2Converged recursive
  where dur = durFromRhythmic rhy t
        x1Converged = calculateStartConvergent defVoiceInSecs cTo dur cFrom
        x2Converged = x1Converged + dur

findX1AndX2 mapa eval (Temporal (Converge convergedKey cTo cFrom t) rhy _) xs = findX1AndX2 mapa eval convergedRecursive xs'
  where convergedRecursive = fromMaybe (Temporal (Kairos 0.0 0.0) O false) $ M.lookup convergedKey mapa
        dur = durFromRhythmic rhy t
        xs' = Cons (Triplet dur cTo cFrom) xs
        


data Triplet = Triplet Number Number Number

instance tripletShow :: Show Triplet where
    show (Triplet x y z) = "triplet " <> show x <> " " <> show y <> " " <> show z

fst3:: Triplet -> Number
fst3 (Triplet x _ _) = x

snd3:: Triplet -> Number
snd3 (Triplet _ y _) = y

thrd:: Triplet -> Number
thrd (Triplet _ _ z) = z

-- check2 :: Map String Temporal -> List String -> String -> Temporal -> Boolean
-- check2 aMap alreadyRefd aKey (Temporal (Kairos _ _) _ _) = true
-- check2 aMap alreadyRefd aKey (Temporal (Metric _ _ _) _ _) = true
-- check2 aMap alreadyRefd aKey (Temporal (Converge anotherKey _ _ _) _ _) =
--   case lookup anotherKey aMap of
--     Nothing -> false
--     Just anotherValue -> case elem aKey alreadyRefd of
--                            true -> false
--                            false -> check2 aMap (aKey : alreadyRefd) anotherKey anotherValue

-- TODO:
---- tests make all the tests
---- make the Converge polytemporal relation
---- Make the Event visualiser (the events within the voice, this is the one that is easily translated into sound):
      -- So the Tuple Number Number needs to become: Constructor (Tuple Number Number) (List Number), where the list of numbers is the moment in which the event is drawn. 

      -- both events and voices need to be indexed. something like: Ix 0 0, Ix 0 1, Ix 0 2, Ix 0 3 would represent a rhythm like xxxx the voice iteration 0 (first) with the event index 0,  voice iteration 0 with event index 1, etc...



-- durs are in seconds but convergences are in percentage: multiply them
calculateStartConvergent durSecsConverged convergeTo durSecsVoice convergeFrom = startOfVoice
  where cTo = convergeTo * durSecsConverged
        cFrom = convergeFrom * durSecsVoice
        startOfVoice = cTo - cFrom



defVoiceInSecs = durInSecs 1.0 120.0

-- working here

-- make a function where 


--

voiceInWindowUnlooped:: Number -> Number -> Number -> Number -> Array (Tuple Number Number)
voiceInWindowUnlooped x1 x2 ws we 
  | (x1 > we) && (x2 > we) = [Tuple 0.0 0.0] -- program has not passed yet
  | (x1 < ws) && (x2 < ws) = [Tuple 0.0 0.0] -- program already passed
  | ((x1 >= ws) && (x1 < we)) && (x2 > we) = [Tuple x1 we] -- program is visible in its head
  | ((x1 >= ws) && (x1 < we)) && ((x2 < we) && (x2 > ws)) = [Tuple x1 x2] -- program is all visible
  | ((x1) < ws) && ((x2 <= we) && (x2 >= ws)) = [Tuple ws x2] -- program is visible in its tail
  | (x1 < ws) && (x2 > we) = [Tuple ws we] -- program is visible partially because it is bigger than window
  | otherwise = [Tuple x1 2.666]

voiceInWindowLooped:: Number -> Number -> Number -> Number -> Array (Tuple Number Number)
voiceInWindowLooped x1 x2 ws we
  | (x1 > we) && (x2 > we) =  [Tuple 0.0 0.0] 
  | ((x1 > ws) && (x1 <= we)) && (x2 >= we) = if (x1 == we) then [Tuple 0.0 0.0] else [Tuple x1 we] 
  | ((x1 > ws) && (x1 < we)) && (x2 < we) = headShowingAndMultipleVoices x1 x2 we voiceAtWE
    where voiceAtWE = lineToPercentageInSecs (we - x1) (x2 -x1)
  | (x1 < ws) && ((x2 < we) && (x2 > ws)) = findBeats ws we (x2 - x1) x1
  | (x1 < ws) && (x2 > we) = [Tuple ws we]
  | otherwise = findBeats ws we (x2 - x1) x1

headShowingAndMultipleVoices:: Number -> Number -> Number -> Number -> Array (Tuple Number Number)
headShowingAndMultipleVoices x1 x2 we voiceAtWE 
  | x2 == we = [Tuple 0.0 0.0]
  | 0 == (floor voiceAtWE - 1) = concat [first, last]
    where dur = x2 - x1
          first = [Tuple x1 x2]
          last = [Tuple x2 we] 
  | otherwise = concat [[first], middleWholes, [andLast]]
    where dur = x2 - x1
          first = Tuple x1 x2
          middleWholes = fromMaybe [] $ init $ map (\x -> Tuple (x) (x + dur)) $ x2: (scanl (+) x2 $ replicate ((floor voiceAtWE) - 1) dur)
          andLast = (\x -> Tuple (snd x) we) $ fromMaybe (Tuple 0.0 0.0) $ last middleWholes


-- addX2, addElapsing and removeRemanent are only useful for visuals, remove them if need to do triger of sound events

findBeats:: Number -> Number -> Number -> Number -> Array (Tuple Number Number)
findBeats ws we dur x1 = removeRemanent we $ addElapsing ws $ addX2 dur $ findBeats' dur x1 ws we

addX2:: Number -> Array Number -> Array (Tuple Number Number)
addX2 dur xs = map (\x -> Tuple x (x + dur)) xs

-- fromMaybe (Tuple 0.0 0.0) $ A.head [Tuple 0.0 0.1, Tuple 0.5 0.6]
removeRemanent:: Number -> Array (Tuple Number Number) -> Array (Tuple Number Number)
removeRemanent we xs = if snd lasty > we then reverse $ (Tuple (fst lasty) we) : (reverse $ fromMaybe ([Tuple 0.0 0.0]) $ init xs) else xs
  where lasty = fromMaybe (Tuple 0.0 0.0) $ last xs

addElapsing:: Number -> Array (Tuple Number Number) -> Array (Tuple Number Number)
addElapsing ws xs = ((\x -> Tuple ws $ fst x) heady) : xs
  where heady = fromMaybe (Tuple 0.0 0.0) $ head xs


findBeats':: Number -> Number -> Number -> Number -> Array Number
findBeats' dur x1 ws we
    | nextBeat dur x1 ws >= we = fromFoldable []
    | otherwise = nextBeat dur x1 ws : findBeats' dur x1 (ws+dur) we

nextBeat:: Number -> Number -> Number -> Number
nextBeat metre offset ws
    | metre == 0.0 = 0.0
    | otherwise =
        let wsInMetre = ws/metre
            offsetInMetre = decimalPart $ offset/metre
            nextBeatInMetre | offsetInMetre >= (decimalPart wsInMetre) = (toNumber $ floor wsInMetre)  + offsetInMetre
                            | otherwise = (toNumber $ ceil wsInMetre) + offsetInMetre
        in nextBeatInMetre * metre


decimalPart:: Number -> Number
decimalPart x = x - (wholePart x)

wholePart:: Number -> Number 
wholePart x = toNumber $ floor x

lineToPercentageInSecs:: Number -> Number -> Number
lineToPercentageInSecs moment dur = moment / dur

  
type Coordinate = {
  x1:: Number,
  x2:: Number,
  y:: Number
  }


-- tests for voiceInWindowEvalUnlooped:
-- Eval Unlooped if voice is in front window span:
test1 = voiceInWindowUnlooped 3.5 4.5 1.0 2.0 -- must be 0.0 0.0

-- if voice is past window span:
test2 = voiceInWindowUnlooped 1.5 2.5 3.0 4.0 -- must be 0.0 0.0

-- if voice is showing a segment by ws
test3 = voiceInWindowUnlooped 1.5 2.5 2.0 3.0 -- must be

-- if voice is showing a segment by we
test4 = voiceInWindowUnlooped 2.5 3.5 2.0 3.0 -- must be 

-- if voice is bigger than window span
test5 = voiceInWindowUnlooped 2.5 4.5 3.0 4.0 -- must be 

-- if voice is within window span
test6 = voiceInWindowUnlooped 1.5 2.5 1.0 3.0 -- must be 

-- Kairos looped

-- if voice is in front of window
test7 = voiceInWindowLooped 3.5 4.5 1.0 2.0 -- must be 0.0 0.0

-- if voice is past window span:
test8 = voiceInWindowLooped 1.5 2.5 3.0 4.0 -- must be 3.0 to 3.5 and 3.5 4.0

-- -- if mutliple 
-- test9 =

-- the subdivision case is still missing
durFromRhythmic:: Rhythmic -> Number -> Number
durFromRhythmic X tempo = durInSecs 1.0 tempo 
durFromRhythmic O tempo = durInSecs 1.0 tempo
durFromRhythmic (Sd rhy) tempo = durInSecs 1.0 tempo
durFromRhythmic (Repeat xs n) tempo = (durFromRhythmic xs tempo) * (toNumber n) 
durFromRhythmic (Rhythmics xs) tempo = sum $ map (\x -> durFromRhythmic x tempo) xs


convergeMetric moment dur2 tempo2 cF = converge moment 1.0 120.0 0.0 dur2 tempo2 cF

-- output: Tuple voice1PercentageAtMoment voice2PercentageAtMoment 
converge:: Number -> Number -> Number -> Number -> Number -> Number -> Number -> Tuple Number Number
converge moment dur1 tempo1 convergedAt dur2 tempo2 convergingFrom = Tuple (conv1*dur1) $ (conv2 + (lineToPercentage ((convergingFrom*durInSecs dur2 tempo2) - (convergedAt*durInSecs dur1 tempo1)) dur2 tempo2)) * dur2
    where conv1 = lineToPercentage moment dur1 tempo1
          conv2 = lineToPercentage moment dur2 tempo2

findPoint:: Number -> Number -> Number -> Number -> Number
findPoint moment point dur tempo = ((durInSecs dur tempo)*point) - moment

lineToPercentage:: Number -> Number -> Number -> Number
lineToPercentage moment dur tempo = moment / durInSecs dur tempo

-- recibe duracion de la linea en unidades de tempo y tempo y vomita duracion de la linea en secs  
durInSecs:: Number -> Number -> Number
durInSecs dur tempo = dur * (bpmToDur tempo)





-- this funcas use a Tuple that represents  a voice:

-- output: Tuple voice1PercentageAtMoment voice2PercentageAtMoment 
converge':: Number -> Tuple Number Number -> Number -> Tuple Number Number -> Number -> Tuple Number Number
converge' moment line1 convergedAt line2 convergingFrom = Tuple conv1 (conv2 + (lineToPercentage' ((convergingFrom*durInSecs' line2) - (convergedAt*durInSecs' line1)) line2))
    where conv1 = lineToPercentage' moment line1
          conv2 = lineToPercentage' moment line2

findPoint':: Number -> Number -> Tuple Number Number -> Number
findPoint' moment point line = ((durInSecs' line)*point) - moment

lineToPercentage':: Number -> Tuple Number Number -> Number
lineToPercentage' moment line = moment / durInSecs' line

durInSecs':: Tuple Number Number -> Number
durInSecs' line = fst line * (bpmToDur (snd line))

line1' = Tuple 12.0 90.0 -- 13 time untis at 90 bpm

line2' = Tuple 12.0 120.0 -- 17 time untis at 135 bpm

bpmToFreq bpm = (1.0/60.0)* bpm

freqToDur freq = 1.0 / freq

bpmToDur bpm = 1.0 / bpmToFreq bpm











-- -- Estuary Tempo Voice: _ | x :|
-- -- Map String Temporal
-- -- data Temporal = Temporal Polytemporal Rhythmic Boolean
-- -- data Polytemporal = Kairos Number | Metric Number Number | Converge String Number Number 








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
