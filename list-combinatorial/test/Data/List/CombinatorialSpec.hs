module Data.List.DigitSpec (spec) where

spec :: Spec
spec = do
    describe "rotate" rotateSpec
    describe "rotations" rotationsSpec
    describe "combinationsOfSize" combinationsOfSizeSpec
    describe "permutationsOfSize" rotationsOfSizeSpec
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
