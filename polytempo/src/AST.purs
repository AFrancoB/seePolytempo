module AST (Rhythmic(..), Polytemporal(..), Temporal(..), Event(..), Onset(..), Index(..), Indexer(..), CPAlign(..), Triplet(..), Coordinate(..), fst3, snd3, thrd, showEventIndex, showStructureIndex) where

import Prelude

import Data.List
import Data.String as Str

data Temporal = Temporal Polytemporal Rhythmic Boolean

instance temporalShow :: Show Temporal where
    show (Temporal x y z) = show x <> " " <> show y <> (if z then " looped" else " unlooped")

data Polytemporal = 
  Kairos Number Number | -- last arg is tempo -- Arg: universal time unit (miliseconds and datetime in purs)
  -- Kairos starts a program at evaluation time (or as soon as possible), no underlying grid
  Metric Number Number Number | -- starts a program attached to a default underlying voice (a tempo grid basically) first number is the point to where the new voice will converge, second number is the point from which it converges. first _ is 0 and second _ is 0 (so both voices align at index 0)
  Converge String Number Number Number -- Args: String is the voice identifier, convergAt (where this voice converges with the identified voice) and convergedFrom (the point of this voice that converges with the identified voice)
  -- Converge starts a program in relationship with another voice

instance polytemporalShowInstance :: Show Polytemporal where
  show (Kairos timemark tempo) = "kairos: " <> show timemark <> " tempo: " <> show tempo
  show (Metric cTo cFrom t) = "voice aligns with metric at "<>show cTo<>" from "<>show cFrom <> " tempo: " <> show t
  show (Converge voice cTo cFrom t) = "voice aligns with "<>show voice<>" at "<>show cTo<>" from "<>show cFrom <> " tempo: " <> show t


data Rhythmic =  -- whenPosix, thats it
  X | -- x
  O |
  Sd Rhythmic | -- [x]
  Repeat Rhythmic Int |
  Rhythmics (List Rhythmic) -- xoxo
-- Bjorklund

instance Show Rhythmic where
  show X = "x"
  show O = "o"
  show (Sd xs) = "[" <> show xs <> "]"
  show (Repeat xs n) = "!" <> show xs <> "#" <> show n
  show (Rhythmics xs) = show xs

-- CPAlign will provide a convergence point in relation to a part of the program.
-- mod 4 will align a cp with the next voice start multiple of 4. The convergenceTo value with 'mod 4' will converge to the other voice at the next voice muliplte of 4. If this would be the convergenceFrom, the voice will align to the other voice from its next voice multiple of 4.

-- data Align = Mod Number Number | Mod' Number | Snap Number | Snap' Number | Origin Number  -- this is the goal

data CPAlign = Mod Int Number | Snap Number | Origin  -- this is the first stage

instance Show CPAlign where
  show (Mod m o) = "cp after first multiple of " <> show m <> " ahead"
  show (Snap o) = "cp at closest"
  show  Origin = "cp at origin"

-- Aligners:
---- Mod Multiple Offset (next start of voice/event multiple of N with an offset number becomes voice 0)
---- Mod' Multiple Offset (closest multiple, can be in the past already)
---- Snap cp happens at closest voice or event.
---- Origin will align the cp at 0 (1st of January, 1970: 12:00 am)


data Indexer = Structure Int (Array Int) CPAlign | Process Int CPAlign

instance Show Indexer where
  show (Structure x xs a) = show x <>"-"<> result <> " " <> show a
      where subdivisions = foldl (<>) "" $ map (\x -> show x <> ".") xs
            result = Str.take (Str.length subdivisions - 1) subdivisions
  show (Process e a) = show e <> " " <> show a


data Event = Event Onset Index

instance Show Event where
    show (Event o i) =  show o <> " " <> show i

showEventIndex (Index _ _ n) = show n 

showStructureIndex (Index x xs _) = show x <>"-"<> result
      where subdivisions = foldl (<>) "" $ map (\x -> show x <> ".") xs
            result = Str.take (Str.length subdivisions - 1) subdivisions

data Onset = Onset Boolean Number

instance Show Onset where
    show (Onset true n) =  "(X" <> " beatPos:" <> (Str.take 8 $ show n) <>")"
    show (Onset false n) = "(O" <> " beatPos:" <> (Str.take 8 $ show n) <>")"

instance Ord Onset where
    compare (Onset bool1 pos1) (Onset bool2 pos2) = pos1 `compare` pos2  

instance Eq Onset where 
    eq (Onset bool1 pos1) (Onset bool2 pos2) = pos1 == pos2


data Index = Index Int (Array Int) Int

instance indexShow :: Show Index where
    show (Index x xs n) = show x <>"-"<> result <> " (" <> (Str.take 8 $ show n) <> ")"
      where subdivisions = foldl (<>) "" $ map (\x -> show x <> ".") xs
            result = Str.take (Str.length subdivisions - 1) subdivisions

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
  y:: Number,
  label:: String,
  onset:: Boolean
  }


data Aligner = Eval Number | Ordinary Number 
