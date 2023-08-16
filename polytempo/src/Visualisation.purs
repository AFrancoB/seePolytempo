module Visualisation (voiceInWindowLooped, processManyVoices,Onset(..), eventsDurations, Event(..), Index(..), test1, test2, test3, test4, test5, test6,test7, test8, test9, drawProgram) where

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
import Data.Either
import Data.Map as M
import Data.Foldable (sum)
import Data.Int
import Data.FunctorWithIndex
import Data.Array (filter,fromFoldable,(!!), zipWith, replicate, concat, (..), (:), init, tail, last,head,reverse,zip)
import Data.List
import Data.Number.Format (toString)
import Data.Traversable (scanl)
import Data.List (fromFoldable,concat,zip,zipWith,length,init) as Li
import Data.Ord (signum)
import Data.String as Str

import Svg.Parser

import Parsing
import Parser
import Rhythm


-- drawProgram:: M.Map String Temporal -> Number -> Number -> Number -> HH.HTML p i
drawProgram mapa ws we eval = svgNodeToHtml $ svgFrame ws we h (calculateVoiceSVG $ mapToVoiceCoordinates mapa ws we eval) $ calculateEventSVG $ mapToEventCoordinates mapa ws we eval
  where h = toNumber $ length $ M.values mapa
------svgNodeToHtml:: forall p i. SvgNode -> HH.HTML p i

svgFrame:: Number -> Number -> Number -> Array SvgNode -> Array SvgNode -> SvgNode
svgFrame ws we h voices events = SvgElement { 
    name: "svg"
  , attributes: Li.fromFoldable [ 
      SvgAttribute "xmlns" "http://www.w3.org/2000/svg"
    , SvgAttribute  "viewBox" (show ws <>" -1 " <> show (we - ws) <> " " <> show (h + 1.0))
    , SvgAttribute "preserveAspectRatio" "none"
    , SvgAttribute "height" "500"
    , SvgAttribute "width" "1000"
    ]
  , children: Li.concat $ Li.fromFoldable [Li.fromFoldable [defs], Li.fromFoldable events, Li.fromFoldable voices, Li.fromFoldable [background ws we h]]
      
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
    , SvgAttribute "fill" "red"
    ]
  , children: Li.fromFoldable []
}

calculateVoiceSVG:: Array Coordinate -> Array SvgNode
calculateVoiceSVG coords = map (\ x -> drawVoice x.x1 x.x2 x.y 0.5) coords

drawVoice:: Number -> Number -> Number -> Number -> SvgNode
drawVoice x1 x2 y wStroke = SvgElement {name: "rect"
  , attributes: Li.fromFoldable [ 
      SvgAttribute "x" $ toString x1
    , SvgAttribute "width" $ toString $ x2 - x1
    , SvgAttribute "y" $ toString y
    , SvgAttribute "height" $ toString 0.1
    , SvgAttribute "fill" "url(#grad)"
    , SvgAttribute "opacity" "0.75"]
  , children: Li.fromFoldable []
}

calculateEventSVG:: Array Coordinate -> Array SvgNode
calculateEventSVG coords = map (\ x -> drawEvent x.x1 x.x2 x.y 0.5) coords

--  <circle cx="50" cy="50" r="40" stroke="black" stroke-width="3" fill="red" />

drawEvent:: Number -> Number -> Number -> Number -> SvgNode
drawEvent x1 x2 y wStroke = SvgElement {name: "circle"
  , attributes: Li.fromFoldable [ 
      SvgAttribute "cx" $ toString x1
    , SvgAttribute "cy" $ toString (y + 0.05)
    , SvgAttribute "r" $ toString 0.05
    , SvgAttribute "fill" "black"
    , SvgAttribute "opacity" "0.85"]
  , children: Li.fromFoldable []
}

-- svg stuff above this point
mapToEventCoordinates:: M.Map String Temporal -> Number -> Number -> Number -> Array Coordinate
mapToEventCoordinates mapa ws we eval = coords
  where calculated = calculateEventsX1 mapa ws we eval
        vals = M.values calculated
        len = length vals
        yTups = Li.zip (range 0 len) vals  -- List (Tuple Y (Array X1))
        toCoordinate item = map (\x -> {x1: x, x2: 0.0, y: toNumber $ fst item} ) $ snd item
        coords = concat $ fromFoldable $ map (\x -> toCoordinate x) yTups

calculateEventsX1:: M.Map String Temporal -> Number -> Number -> Number -> M.Map String (Array  Number)
calculateEventsX1 mapa ws we eval = mapWithIndex (calculateEventX1 mapa ws we eval) mapa 

calculateEventX1:: M.Map String Temporal -> Number -> Number -> Number -> String -> Temporal -> Array Number  -- Tuple X1 X2
calculateEventX1 mapa ws we eval aKey (Temporal (Kairos asap tempo) rhythmic loop) = if loop then looped else unlooped-- if loop then looped else unlooped
  where x1 = eval + asap -- always the start of the program
        dur = durFromRhythmic rhythmic tempo
        x2 = x1 + dur
        psPercent = fromFoldable $ rhythmicToOnsets rhythmic -- Array Onset
        ps = map (\(Onset bool pos) -> (Onset bool (pos * dur))) psPercent
        xs = map (\ (Onset bool pos) -> x1 + pos) ps
        unlooped = map f $ filter isRight $ map (\x -> eventInWindowUnlooped x ws we) $ fromFoldable xs
        voices = voiceInWindowLooped x1 x2 ws we -- [Tuple x1 x2]
        looped = map f $ filter isRight $ processManyVoices voices dur psPercent

calculateEventX1 mapa ws we eval aKey (Temporal (Metric cTo cFrom t) rhythmic loop) = if loop then looped else unlooped  
  where dur = durFromRhythmic rhythmic t
        x1 = calculateStartConvergent defVoiceInSecs cTo dur cFrom 
        x2 = x1 + dur
        psPercent = fromFoldable $ rhythmicToOnsets rhythmic -- Array Onset
        ps = map (\(Onset bool pos) -> (Onset bool (pos * dur))) psPercent
        xs = map (\ (Onset bool pos) -> x1 + pos) ps
        unlooped = map f $ filter isRight $ map (\x -> eventInWindowUnlooped x ws we) $ fromFoldable xs
        voices = voiceInWindowLooped x1 x2 ws we -- [Tuple x1 x2]
        looped = map f $ filter isRight $ processManyVoices voices dur psPercent

calculateEventX1 mapa ws we eval aKey (Temporal (Converge convergedKey cTo cFrom t) rhythmic loop) = if loop then looped else unlooped
  where dur = durFromRhythmic rhythmic t
        (Tuple x1 x2) = convergeFunc mapa convergedKey eval dur cTo cFrom
        psPercent = fromFoldable $ rhythmicToOnsets rhythmic -- Array Onset
        ps = map (\(Onset bool pos) -> (Onset bool (pos * dur))) psPercent
        xs = map (\ (Onset bool pos) -> x1 + pos) ps
        unlooped = map f $ filter isRight $ map (\x -> eventInWindowUnlooped x ws we) $ fromFoldable xs
        voices = voiceInWindowLooped x1 x2 ws we -- [Tuple x1 x2]
        looped = map f $ filter isRight $ processManyVoices voices dur psPercent

-- this two will work only in cases where the start of the voices is showing
processManyVoices:: Array (Tuple Number Number) -> Number -> Array Onset -> Array (Either String Number) 
processManyVoices voices voiceDur eventsPercen = concat $ map (\voice -> processEventsInVoice voice voiceDur eventsPercen) voices

processEventsInVoice:: Tuple Number Number -> Number -> Array Onset -> Array (Either String Number)
processEventsInVoice voice voiceDur eventsPercen = map (\(Onset b pos) -> x1VisibleInWindow voice voiceDur pos) eventsPercen

x1VisibleInWindow:: Tuple Number Number -> Number -> Number -> Either String Number
x1VisibleInWindow (Tuple x1 x2) voiceDur x = eventInW
  where eventPos = x1 + (x * voiceDur)
        eventInW = if eventPos < x2 then Right eventPos else Left "nothing"

firstSemiVisible:: Tuple Number Number -> Number -> Number
firstSemiVisible (Tuple x1 x2) durVoice = cutPointInPercen
  where cutPointInPercen = (durVoice - (durVoice - (x2 - x1))) / durVoice
-- a number representing a percentage below which everything is filtered out


-- testa:: M.Map String Temporal -> 
-- testa pr ws we eval = (\m -> calculateEventX1 m ws we eval "v0" <$> val) parsed
--   where parsed = runParser pr polytemporal 
--         val = fromMaybe (Temporal (Kairos 0.0 0.0) O false) <$> M.lookup "v0" <$> parsed

f:: Either String Number -> Number
f (Left x) = 0.0
f (Right x) = x

eventInWindowUnlooped:: Number -> Number -> Number -> Either String Number
eventInWindowUnlooped x ws we 
  | x > we = Left "nothing"
  | x < ws = Left "nothing"
  | otherwise = Right x

---

mapToVoiceCoordinates:: M.Map String Temporal -> Number -> Number -> Number -> Array Coordinate
mapToVoiceCoordinates mapa ws we eval = coords
  where calculated = calculateVoicesX1X2 mapa ws we eval
        vals = M.values calculated
        len = length vals
        yTups = Li.zip (range 0 len) vals  -- List (Tuple Y (Array (Tuple X1 X2)))
        toCoordinate item = map (\x -> {x1: fst x, x2: snd x, y: toNumber $ fst item} ) $ snd item
        coords = concat $ fromFoldable $ map (\x -> toCoordinate x) yTups


calculateVoicesX1X2:: M.Map String Temporal -> Number -> Number -> Number -> M.Map String (Array (Tuple Number Number))
calculateVoicesX1X2 mapa ws we eval = mapWithIndex (calculateVoiceX1X2 mapa ws we eval) mapa 


-- make a test
calculateVoiceX1X2:: M.Map String Temporal -> Number -> Number -> Number -> String -> Temporal -> Array (Tuple Number Number) -- Tuple X1 X2
calculateVoiceX1X2 mapa ws we eval aKey (Temporal (Kairos asap tempo) rhythmic loop) = if loop then looped else unlooped
  where startOfVoice = eval + asap -- always the start of the program
        dur = durFromRhythmic rhythmic tempo -- dur of voice in seconds
        unlooped = filter (\x -> x /= (Tuple 0.0 0.0)) $ voiceInWindowUnlooped startOfVoice (startOfVoice+dur) ws we
        looped = filter (\x -> x /= (Tuple 0.0 0.0)) $ voiceInWindowLooped startOfVoice (startOfVoice+dur) ws we

calculateVoiceX1X2 mapa ws we eval aKey (Temporal (Metric cTo cFrom t) rhythmic loop) = if loop then looped else unlooped  
  where dur = durFromRhythmic rhythmic t
        startOfVoice = calculateStartConvergent defVoiceInSecs cTo dur cFrom 
        unlooped = filter (\x -> x /= (Tuple 0.0 0.0)) $ voiceInWindowUnlooped startOfVoice (startOfVoice+dur) ws we
        looped = filter (\x -> x /= (Tuple 0.0 0.0)) $ voiceInWindowLooped startOfVoice (startOfVoice+dur) ws we

calculateVoiceX1X2 mapa ws we eval aKey (Temporal (Converge convergedKey cTo cFrom t) rhythmic loop) = if loop then looped else unlooped
  where dur = durFromRhythmic rhythmic t
        (Tuple x1 x2) = convergeFunc mapa convergedKey eval dur cTo cFrom
        unlooped = filter (\x -> x /= (Tuple 0.0 0.0)) $ voiceInWindowUnlooped x1 x2 ws we
        looped = filter (\x -> x /= (Tuple 0.0 0.0)) $ voiceInWindowLooped x1 x2 ws we


convergeFunc:: M.Map String Temporal -> String -> Number -> Number -> Number -> Number -> Tuple Number Number
convergeFunc mapa convergedKey eval dur cTo cFrom = findX1AndX2ForConverge mapa eval convergedValue (Cons (Triplet dur cTo cFrom) Nil)
  where cFromInSecs = dur * cFrom 
        convergedValue = fromMaybe (Temporal (Kairos 0.0 0.0) O false) $ M.lookup convergedKey mapa

findX1AndX2ForConverge:: M.Map String Temporal -> Number -> Temporal -> List Triplet -> Tuple Number Number
findX1AndX2ForConverge mapa eval (Temporal (Kairos asap t) rhy _) recursive = recursBack x1Converged x2Converged recursive
  where x1Converged = eval + asap -- always the start of the program
        x2Converged = x1Converged + (durFromRhythmic rhy t)

findX1AndX2ForConverge mapa eval (Temporal (Metric cTo cFrom t) rhy _) recursive = recursBack x1Converged x2Converged recursive
  where dur = durFromRhythmic rhy t
        x1Converged = calculateStartConvergent defVoiceInSecs cTo dur cFrom
        x2Converged = x1Converged + dur

findX1AndX2ForConverge mapa eval (Temporal (Converge convergedKey cTo cFrom t) rhy _) xs = findX1AndX2ForConverge mapa eval convergedRecursive xs'
  where convergedRecursive = fromMaybe (Temporal (Kairos 0.0 0.0) O false) $ M.lookup convergedKey mapa
        dur = durFromRhythmic rhy t
        xs' = Cons (Triplet dur cTo cFrom) xs

recursBack:: Number -> Number -> List Triplet -> Tuple Number Number
recursBack x1Converged x2Converged (Nil) = Tuple x1Converged x2Converged 
recursBack x1Converged x2Converged (Cons x xs) = 
  let convergedTo = (x2Converged - x1Converged) * (snd3 x)
      convergingFrom = (fst3 x) * (thrd x) 
      convergencePoint = x1Converged + convergedTo
      convergingX1 = convergencePoint - convergingFrom
      convergingX2 = convergingX1 + (fst3 x)
  in recursBack convergingX1 convergingX2 xs

-- TODO:
---- tests make all the tests
---- Make the Event visualiser (the events within the voice, this is the one that is easily translated into sound):
      -- So the Tuple Number Number needs to become: Constructor (Tuple Number Number) (List Number), where the list of numbers is the moment in which the event is drawn. 

      -- both events and voices need to be indexed. something like: Ix 0 0, Ix 0 1, Ix 0 2, Ix 0 3 would represent a rhythm like xxxx the voice iteration 0 (first) with the event index 0,  voice iteration 0 with event index 1, etc...

-- durs are in seconds but convergences are in percentage: multiply them
calculateStartConvergent durSecsConverged convergeTo durSecsVoice convergeFrom = startOfVoice
  where cTo = convergeTo * durSecsConverged
        cFrom = convergeFrom * durSecsVoice
        startOfVoice = cTo - cFrom


defVoiceInSecs = durInSecs 1.0 120.0

--- loop or unlooped:

voiceInWindowUnlooped:: Number -> Number -> Number -> Number -> Array (Tuple Number Number)
voiceInWindowUnlooped x1 x2 ws we 
  | (x1 > we) && (x2 > we) = [Tuple 0.0 0.0] -- program has not passed yet
  | (x1 < ws) && (x2 < ws) = [Tuple 0.0 0.0] -- program already passed
  | ((x1 >= ws) && (x1 < we)) && (x2 > we) = [Tuple x1 we] -- program is visible in its head
  | ((x1 >= ws) && (x1 < we)) && ((x2 <= we) && (x2 > ws)) = [Tuple x1 x2] -- program is all visible
  | ((x1) < ws) && ((x2 <= we) && (x2 >= ws)) = [Tuple ws x2] -- program is visible in its tail
  | (x1 < ws) && (x2 > we) = [Tuple ws we] -- program is visible partially because it is bigger than window
  | otherwise = [Tuple x1 2.666]

voiceInWindowLooped:: Number -> Number -> Number -> Number -> Array (Tuple Number Number)
voiceInWindowLooped x1 x2 ws we
  | (x1 > we) && (x2 > we) =  [Tuple 0.0 0.0] 
  | ((x1 > ws) && (x1 <= we)) && (x2 >= we) = if (x1 == we) then [Tuple 0.0 0.0] else [Tuple x1 we] 
  | ((x1 > ws) && (x1 < we)) && (x2 < we) = headShowingAndMultipleVoices x1 x2 we voiceAtWE
    where voiceAtWE = lineToPercentageInSecs (we - x1) (x2 - x1)
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


-- tests for:
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
test8 = voiceInWindowLooped 1.5 2.5 1.0 2.0 -- must be 3.0 to 3.5 and 3.5 4.0

-- -- if mutliple         -- x1  x2  ws we
test9 = voiceInWindowLooped 1.5 2.5 1.0 3.0

-- the subdivision case is still missing
durFromRhythmic:: Rhythmic -> Number -> Number
durFromRhythmic X tempo = durInSecs 1.0 tempo 
durFromRhythmic O tempo = durInSecs 1.0 tempo
durFromRhythmic (Sd rhy) tempo = durInSecs 1.0 tempo
durFromRhythmic (Repeat rhy n) tempo = (durFromRhythmic rhy tempo) * (toNumber n) 
durFromRhythmic (Rhythmics xs) tempo = sum $ map (\x -> durFromRhythmic x tempo) xs


data Onset = Onset Boolean Number 

instance Show Onset where
    show (Onset true n) =  "(X" <> " beatPos:" <> (Str.take 8 $ show n) <> ")"
    show (Onset false n) = "(O" <> " beatPos:" <> (Str.take 8 $ show n) <> ")"

instance Ord Onset where
    compare (Onset bool1 pos1) (Onset bool2 pos2) = pos1 `compare` pos2  

instance Eq Onset where 
    eq (Onset bool1 pos1) (Onset bool2 pos2) = pos1 == pos2

rhythmicToRefrainDuration:: Rhythmic -> Number -- does not need Tempo...?
rhythmicToRefrainDuration X = 1.0
rhythmicToRefrainDuration O = 1.0
rhythmicToRefrainDuration (Sd xs) = 1.0
rhythmicToRefrainDuration (Repeat xs n) = foldl (+) 0.0 x
    where x = replicate n $ rhythmicToRefrainDuration xs
rhythmicToRefrainDuration (Rhythmics xs) = foldl (+) 0.0 x
    where x = map (\x -> rhythmicToRefrainDuration x) xs

rhythmicToOnsets:: Rhythmic -> List Onset
rhythmicToOnsets rhy = 
    let refrainDur = rhythmicToRefrainDuration rhy
        rhythmicSegments = eventsDurations 1.0 rhy
        durInPercentOfEvents = Cons 0.0 $ (fromMaybe (Li.fromFoldable []) $ Li.init $ scanl (+) 0.0 $ map (\x -> x/refrainDur) $ getDur <$> rhythmicSegments) -- List Number
    in Li.zipWith (\x y -> Onset x y) (getBool <$> rhythmicSegments) durInPercentOfEvents -- we need to keep the XO

eventsDurations:: Number -> Rhythmic -> List Onset
eventsDurations pos X =  Li.fromFoldable [Onset true pos]
eventsDurations pos O =  Li.fromFoldable [Onset false pos]
eventsDurations dur (Sd xs) = eventsDurations' dur xs
eventsDurations dur (Repeat xs n) = Li.concat $ map (\x -> eventsDurations dur x) $ Li.fromFoldable $ replicate n xs
eventsDurations dur (Rhythmics xs) = Li.concat $ map (\x-> eventsDurations dur x) xs

getDur:: Onset -> Number
getDur (Onset _ x) = x

getBool:: Onset -> Boolean 
getBool (Onset x _) = x

-- data Refrain = Refrain Int Number MetricStructure EventCount
eventsDurations':: Number -> Rhythmic -> List Onset
eventsDurations' dur X = Li.fromFoldable [Onset true dur]
eventsDurations' dur O = Li.fromFoldable [Onset false dur]
eventsDurations' dur (Sd xs) = eventsDurations' dur xs
eventsDurations' dur (Repeat xs n) = Li.concat $ map (\x -> eventsDurations' newDur x) $ Li.fromFoldable $ replicate n xs
    where newDur = dur / (toNumber n)
eventsDurations' dur (Rhythmics xs) = Li.concat $ map (\x-> eventsDurations' newDur x) xs
    where newDur = dur / (toNumber $ length xs)


-- -- recibe duracion de la linea en unidades de tempo y tempo y vomita duracion de la linea en secs  
durInSecs:: Number -> Number -> Number
durInSecs dur tempo = dur * (bpmToDur tempo)

bpmToFreq bpm = (1.0/60.0)* bpm

freqToDur freq = 1.0 / freq

bpmToDur bpm = 1.0 / bpmToFreq bpm


data Index = Index Int Int (Array Int) 

instance indexShow :: Show Index where
    show (Index x y z) = "index " <> show x <> " " <> show y <> " " <> show z

data Event = Event Number Boolean Index

instance eventShow :: Show Event where
    show (Event x y z) = "event " <> show x <> " " <> show y <> " " <> show z

data Triplet = Triplet Number Number Number

instance tripletShow :: Show Triplet where
    show (Triplet x y z) = "triplet " <> show x <> " " <> show y <> " " <> show z

fst3:: Triplet -> Number
fst3 (Triplet x _ _) = x

snd3:: Triplet -> Number
snd3 (Triplet _ y _) = y

thrd:: Triplet -> Number
thrd (Triplet _ _ z) = z

type Coordinate = {
  x1:: Number,
  x2:: Number,
  y:: Number
  }



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


----

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
