{-# LANGUAGE TemplateHaskell, FlexibleInstances, QuasiQuotes #-}

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.TH 
import Test.Tasty.HUnit

import Lucifer

import Data.Word
import Data.Maybe

import Language.Literals.Binary

main = $(defaultMainGenerator)

instance Arbitrary Digit where
    arbitrary = QC.elements (map Digit [0, 2^7-1])

prop_fromList :: Digit -> Bool
prop_fromList digit = (fromList . toList) digit == digit 

prop_fromString :: Digit -> Bool
prop_fromString digit = (fromString . toString) digit == Just digit

case_allWorks = do 
    let evaluated = map (\d -> allCandidates d eight none) digits
    let predicted = map (: []) digits
    evaluated @=? predicted

case_trafficLightDoesntLight =  
    allCandidates none none none @=? digits

case_everythingBroken = do
    let evaluated = map (\d -> realWorldDetailedCandidates d none eight) digits
    let predicted = replicate 10 Nothing
    evaluated @=? predicted

case_wannaBeEight1 = do
    let evaluated = realWorldDetailedCandidates (Digit [b| 1111101 |]) none none
    let predicted = Just ([eight], Digit [b| 1111101 |], Digit [b| 0000010 |])
    evaluated @=? predicted

case_wannaBeEight2 = do
    let evaluated = realWorldDetailedCandidates (Digit [b| 1111100 |]) none none
    let predicted = Just ([eight], Digit [b| 1111100 |], Digit [b| 0000011 |])
    evaluated @=? predicted

