module Calculations (mapToVoiceCoordinates, mapToOnsetCoordinates) where

import Prelude

import Effect (Effect)

import Data.Tuple
import Data.Maybe
import Data.Either
import Data.Map as M
import Data.Foldable (sum)
import Data.Int
import Data.FunctorWithIndex (mapWithIndex)
import Data.Array (filter,fromFoldable,(!!), zipWith, replicate, concat, (..), (:), init, tail, last,head,reverse,zip, snoc)
import Data.List
import Data.Traversable (scanl)
import Data.List (fromFoldable,concat,zip,zipWith,length,init) as L

import AST
import Parsing
import Parser
import Rhythm


-- glosario:
---- Layer is a voice in a sequence (looped). So, a layer defines an array (sequence) of voices that are identical except their position in time (and their voice index)
---- Voice is a rhythmic pattern of onsets (other possible names: refrain, estribillo, ritornello, period, idea, phrase, frase, shape, figure, contorno, ...)
---- Onset a moment in time when a sound is instantiated

---- An Event is an Onset with an Index

---- Index is a mark that allows me to identify the position of the onset so sounds and sound characteristics can be attached to it
---- process-oriented index: an int identifier for each onset on a flow of onsets. 
---- eventIndex is the way I will reger to process oriented indexes
---- structure-oriented index: an int identifier for each voice on a layer and an array to identifier internal events in a voice: The head is the 'natural' subdivisions of the voice, each new element in the array is a new subdivision
---- a structure oriented index has a voice index and a structure index. A voice index is an Int while the Structure Index is an Array Int. The notation I have made for the structure oriented index is: 3-0.2.4  to the left of the (-) is the voice index and to the right of it is the event position in the rhythmic idea. The head of the array is the top level of the nested subdivisions and the last is the deepest level of the subdivisions.  
       

mapToOnsetCoordinates:: M.Map String Temporal -> Number -> Number -> Number -> Array Coordinate
mapToOnsetCoordinates mapa ws we eval = coords
  where calculated = calculateOnsets mapa ws we eval --Map Str (Array Event)
        vals = M.values calculated
        getPos v = map f v
          where f e = map (\(Event (Onset _ x) _) -> x) e
        getPosAndLabel v = map f v
          where f e = map (\(Event (Onset _ x) i) -> Tuple x (showStructureIndex i)) e
        len = length $ getPos vals
        yTups = L.zip (range 0 len) $ getPosAndLabel vals  -- List (Tuple Y (Array (Tuple X1 label)))
        toCoordinate item = map (\x -> {x1: fst x, x2: 0.0, y: toNumber $ fst item, label: snd x, onset: false} ) $ snd item
        coords = concat $ fromFoldable $ map (\x -> toCoordinate x) yTups

calculateOnsets:: M.Map String Temporal -> Number -> Number -> Number -> M.Map String (Array  Event)
calculateOnsets mapa ws we eval = mapWithIndex (calculateOnset mapa ws we eval) mapa 

calculateOnset:: M.Map String Temporal -> Number -> Number -> Number -> String -> Temporal -> Array Event  
calculateOnset mapa ws we eval aKey (Temporal (Kairos asap tempo) rhythmic loop) = if loop then looped else unlooped-- if loop then looped else unlooped
  where dur = durFromRhythmic rhythmic tempo -- number
        x1 = eval + asap -- always the start of the program
        x2 = x1 + dur
        blocks = getBlocks (ws - dur) we x1 dur -- Array Number
        onsets = onsetsFromBlocks blocks (fromFoldable $ rhythmicToOnsets rhythmic) dur -- Array Onset --- absolute position
        indexes = getIndexes rhythmic (ws - dur) we x1 dur -- Array Index
        events = zipWith Event onsets indexes
        looped = filter (\e -> (posFromEvent e) >= ws && (posFromEvent e) < we) events
        unlooped = eventsInWindowUnlooped events

calculateOnset mapa ws we eval aKey (Temporal (Metric cTo cFrom t) rhythmic loop) = if loop then looped else unlooped  
  where dur = durFromRhythmic rhythmic t
        x1 = calculateStartConvergent defVoiceInSecs cTo dur cFrom 
        x2 = x1 + dur
        blocks = getBlocks (ws - dur) we x1 dur -- here you can extract the Int for index
        onsets = onsetsFromBlocks blocks (fromFoldable $ rhythmicToOnsets rhythmic) dur -- Array Onset --- absolute position
        indexes = getIndexes rhythmic (ws - dur) we x1 dur -- Array Index
        events = zipWith Event onsets indexes
        looped = filter (\e -> (posFromEvent e) >= ws && (posFromEvent e) < we) events
        unlooped = eventsInWindowUnlooped events

calculateOnset mapa ws we eval aKey (Temporal (Converge convergedKey cTo cFrom t) rhythmic loop) = if loop then looped else unlooped
  where dur = durFromRhythmic rhythmic t
        (Tuple x1 x2) = convergeFunc mapa convergedKey eval dur cTo cFrom
        blocks = getBlocks (ws - dur) we x1 dur 
        onsets = onsetsFromBlocks blocks (fromFoldable $ rhythmicToOnsets rhythmic) dur -- Array Onset --- absolute position
        indexes = getIndexes rhythmic (ws - dur) we x1 dur -- Array Index
        events = zipWith Event onsets indexes
        looped = filter (\e -> (posFromEvent e) >= ws && (posFromEvent e) < we) events
        unlooped = eventsInWindowUnlooped events

posFromEvent:: Event -> Number
posFromEvent (Event (Onset _ p) _) = p

-- boolFromEvent:: Event -> Number
-- boolFromEvent (Event (Onset b _) _) = b

-- posFromEvent:: Event -> Number
-- posFromEvent (Event _ (Index v _ _)) = v

-- structFromEvent:: Event -> Number
-- structFromEvent (Event _ (Index _ st _)) = st

-- eventFromEvent:: Event -> Number
-- eventFromEvent (Event _ (Index _ _ e)) = ei


getBlocks:: Number -> Number -> Number -> Number -> Array Number
getBlocks xws we x1 dur = 
  let nOfFstBlock = firstBlock xws x1 dur  -- :: Int
      nOfLstBlock = lastBlock we x1 dur  -- Maybe Int
      nOfBlocks = case nOfLstBlock of 
                    Nothing -> [] --(nOfFstBlock..(nOfFstBlock + 1))
                    (Just n) -> (nOfFstBlock..n) -- [Int]
  in (blockToPos nOfBlocks x1 dur)

blockToPos:: Array Int -> Number -> Number -> Array Number
blockToPos is x1 dur = map (\i -> x1 + ((toNumber i) * dur)) is

firstBlock:: Number -> Number -> Number -> Int
firstBlock _ _ 0.0 = 0
firstBlock xws x1 dur = nOfNxBlock
  where ws' = (xws - x1)/dur 
        nOfNxBlock 
          | ws' < 0.0 = 0
          | otherwise = ceil ws' 

lastBlock:: Number -> Number -> Number -> Maybe Int
lastBlock we x1 dur = nOfLastBlock
  where we' = (we - x1)/dur
        nOfLastBlock 
          -- | x1 + (we'*dur) >= toNumber (floor we') = Nothing
          | x1 + (we'*dur) <= x1 = Nothing 
          | otherwise = Just $ floor we'

onsetsFromBlocks:: Array Number -> Array Onset -> Number -> Array Onset 
onsetsFromBlocks blocks onsets dur = concat $ map (\block -> onsetsFromBlock onsets block dur) blocks

onsetsFromBlock:: Array Onset -> Number -> Number -> Array Onset
onsetsFromBlock onsets block dur = map (\(Onset bool pos) -> Onset bool (block + (pos*dur))) onsets 

-- testa:: M.Map String Temporal -> 
-- testa pr ws we eval = (\m -> calculateEventX1 m ws we eval "v0" <$> val) parsed
--   where parsed = runParser pr polytemporal 
--         val = fromMaybe (Temporal (Kairos 0.0 0.0) O false) <$> M.lookup "v0" <$> parsed

eventsInWindowUnlooped:: Array Event -> Array Event
eventsInWindowUnlooped es = map unEither $ filter isRight $ map (\e -> eventInWindowUnlooped e) es

eventInWindowUnlooped:: Event -> Either String Event
eventInWindowUnlooped (Event onset (Index v st e))
  | v == 0 = Right $ Event onset (Index v st e)
  | otherwise = Left "nothing"

unEither:: Either String Event -> Event
unEither (Left x) = Event (Onset false 0.0) (Index 0 [0] 0)
unEither (Right x) = x

---

mapToVoiceCoordinates:: M.Map String Temporal -> Number -> Number -> Number -> Array Coordinate
mapToVoiceCoordinates mapa ws we eval = coords
  where calculated = calculateVoicesX1X2 mapa ws we eval
        vals = M.values calculated
        len = length vals
        yTups = L.zip (range 0 len) vals  -- List (Tuple Y (Array (Tuple X1 X2)))
        toCoordinate item = map (\x -> {x1: fst x, x2: snd x, y: toNumber $ fst item, label: "", onset: false} ) $ snd item
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
      -- both events and voices need to be indexed. something like: Ix 0 0, Ix 0 1, Ix 0 2, Ix 0 3 would represent a rhythm like xxxx the voice iteration 0 (first) with the event index 0,  voice iteration 0 with event index 1, etc...


calculateStartConvergent:: Number -> Number -> Number -> Number -> Number
calculateStartConvergent durSecsConverged convergeTo durSecsVoice convergeFrom = startOfVoice
  where cTo = convergeTo * durSecsConverged
        cFrom = convergeFrom * durSecsVoice
        startOfVoice = cTo - cFrom


defVoiceInSecs = durInSecs 1.0 120.0 -- this should be taken from estuary's tempo

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

rhythmicToVoiceDuration:: Rhythmic -> Number -- does not need Tempo...?
rhythmicToVoiceDuration X = 1.0
rhythmicToVoiceDuration O = 1.0
rhythmicToVoiceDuration (Sd xs) = 1.0
rhythmicToVoiceDuration (Repeat xs n) = foldl (+) 0.0 x
    where x = replicate n $ rhythmicToVoiceDuration xs
rhythmicToVoiceDuration (Rhythmics xs) = foldl (+) 0.0 x
    where x = map (\x -> rhythmicToVoiceDuration x) xs

rhythmicToOnsets:: Rhythmic -> List Onset
rhythmicToOnsets rhy = 
    let voiceDur = rhythmicToVoiceDuration rhy
        rhythmicSegments = onsetDurations 1.0 rhy
        durInPercentOfEvents = Cons 0.0 $ (fromMaybe (L.fromFoldable []) $ L.init $ scanl (+) 0.0 $ map (\x -> x/voiceDur) $ getDur <$> rhythmicSegments) -- List Number
    in L.zipWith (\x y -> Onset x y) (getBool <$> rhythmicSegments) durInPercentOfEvents -- we need to keep the XO -- THIS gives percentage position within voice, 

onsetDurations:: Number -> Rhythmic -> List Onset
onsetDurations dur X =  L.fromFoldable [Onset true dur]
onsetDurations dur O =  L.fromFoldable [Onset false dur]
onsetDurations dur (Sd xs) = onsetDurations' dur xs
onsetDurations dur (Repeat xs n) = L.concat $ map (\x -> onsetDurations dur x) $ L.fromFoldable $ replicate n xs
onsetDurations dur (Rhythmics xs) = L.concat $ map (\x-> onsetDurations dur x) xs

getDur:: Onset -> Number
getDur (Onset _ x) = x

getBool:: Onset -> Boolean 
getBool (Onset x _) = x

onsetDurations':: Number -> Rhythmic -> List Onset
onsetDurations' dur X = L.fromFoldable [Onset true dur]
onsetDurations' dur O = L.fromFoldable [Onset false dur]
onsetDurations' dur (Sd xs) = onsetDurations' dur xs
onsetDurations' dur (Repeat xs n) = L.concat $ map (\x -> onsetDurations' newDur x) $ L.fromFoldable $ replicate n xs
    where newDur = dur / (toNumber n)
onsetDurations' dur (Rhythmics xs) = L.concat $ map (\x-> onsetDurations' newDur x) xs
    where newDur = dur / (toNumber $ length xs)


getIndexes:: Rhythmic -> Number -> Number -> Number -> Number -> Array Index
getIndexes rhythmic xws we x1 dur = 
  let lenOnset = length $ rhythmicToOnsets rhythmic
      voiceIndexes = getVoiceIndex xws we x1 dur
      structIndexes = rhythmicStructIndex rhythmic [0]
      eventIndexesPerVoice = (0..(lenOnset-1)) 
      eventIndexes = funquilla voiceIndexes eventIndexesPerVoice lenOnset -- Array (Array Int)
  in assambleIndex voiceIndexes structIndexes eventIndexes--{voice: voiceIndexes, struct: structIndexes, events: eventIndexes}

assambleIndex:: Array Int -> Array (Array Int) -> Array (Array Int) -> Array Index
assambleIndex vs st es = concat $ zipWith f vs xs 
  where xs = map (\e -> zip st e) es
        f:: Int -> Array (Tuple (Array Int) Int) -> Array Index
        f v xs = map (\x -> Index v (fst x) (snd x)) xs

funquilla:: Array Int -> Array Int -> Int -> Array (Array Int) 
funquilla voicesIndexes onsetIndexes lenOnsets = map (\voiceIndex -> funquilla' onsetIndexes lenOnsets voiceIndex) voicesIndexes
  where funquilla' onsetIndexes lenOnsets voiceIndex = map (\onsetIndex -> (voiceIndex*lenOnsets)+onsetIndex) onsetIndexes

--- !!!! the Repeat constructor is broken.... no clue how to fix it
rhythmicStructIndex:: Rhythmic -> Array Int -> Array (Array Int)
rhythmicStructIndex X i = [i] 
rhythmicStructIndex O i = [i]
rhythmicStructIndex (Rhythmics xs) i = concat $ map (\(Tuple x i') -> rhythmicStructIndex x [i']) zipped
  where zipped = zip (fromFoldable xs) (0..((L.length xs)-1))
-- rhythmicStructIndex (Repeat rhy n) i = 
rhythmicStructIndex (Sd rhy) i = rhythmicStructIndex' rhy i
rhythmicStructIndex _ _ = [[2666]]

rhythmicStructIndex' X i = [i]
rhythmicStructIndex' O i = [i]
rhythmicStructIndex' (Rhythmics xs) i = concat $ map (\(Tuple x i') -> rhythmicStructIndex' x (snoc i i')) zipped
  where zipped = zip (fromFoldable xs) (0..((L.length xs)-1))
rhythmicStructIndex' (Sd rhy) i = rhythmicStructIndex' rhy i

rhythmicStructIndex' _ i = [snoc i 2666]


getVoiceIndex:: Number -> Number -> Number -> Number -> Array Int -- Index for Voice 
getVoiceIndex xws we x1 dur = 
  let nOfFstBlock = firstBlock xws x1 dur  -- :: Int
      nOfLstBlock = lastBlock we x1 dur  -- Maybe Int
      nOfBlocks = case nOfLstBlock of 
                    Nothing -> []
                    (Just n) -> (nOfFstBlock..n) -- [Int]
  in nOfBlocks


-- -- recibe duracion de la linea en unidades de tempo y vomita duracion de la linea en secs  
durInSecs:: Number -> Number -> Number
durInSecs dur tempo = dur * (bpmToDur tempo)

bpmToFreq bpm = (1.0/60.0)* bpm

freqToDur freq = 1.0 / freq

bpmToDur bpm = 1.0 / bpmToFreq bpm

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
