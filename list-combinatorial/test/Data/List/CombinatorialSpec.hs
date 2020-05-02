module Data.List.CombinatorialSpec (spec) where

import Test.Hspec (Spec, describe)

spec :: Spec
spec = do
    describe "rotate" rotateSpec
    describe "rotations" rotationsSpec
    describe "combinationsOfSize" combinationsOfSizeSpec
    describe "permutationsOfSize" permutationsOfSizeSpec
    describe "derangements" derangementsSpec

rotateSpec :: Spec
rotateSpec = undefined

rotationsSpec :: Spec
rotationsSpec = undefined

combinationsOfSizeSpec :: Spec
combinationsOfSizeSpec = undefined

permutationsOfSizeSpec :: Spec
permutationsOfSizeSpec = undefined

derangementsSpec :: Spec
derangementsSpec = undefined
