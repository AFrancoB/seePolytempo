module Visualisation where

import Prelude

import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

import Data.Array (fromFoldable)
import Data.Tuple

import Svg.Parser

import Parser



-- funca:: Map String Temporal -> Number -> Tuple Number Number
-- funca mapa moment = map 
--     let get first voice, if voice is kairos the align the voice at the right moment.
--     if voice is metric then use converge with the standard voice
--     if the voice is converge then look for the other voice and converge with it.
--     Problem: the converged voiced has to be either kairos, metric or converging so it need to be checked recursively


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
