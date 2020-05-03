module Data.List.CombinatorialSpec (spec) where

import Control.Monad           (forM_, (>=>))
import Data.List               (permutations, sort)
import Test.Hspec              (Spec, describe, it, shouldBe)

import Data.List.Combinatorial (combinationsOfSize, derangements,
                                permutationsOfSize, rotate, rotations)

spec :: Spec
spec = do
    describe "rotate" rotateSpec
    describe "rotations" rotationsSpec
    describe "combinationsOfSize" combinationsOfSizeSpec
    describe "permutationsOfSize" permutationsOfSizeSpec
    describe "derangements" derangementsSpec

rotateSpec :: Spec
rotateSpec = do
    it "empty list, 0 offset" $
        rotate 0 [] `shouldBe` ([] :: [()])
    it "empty list, positive offset" $
        rotate 3 [] `shouldBe` ([] :: [()])
    it "empty list, negative offset" $
        rotate (-2) [] `shouldBe` ([] :: [()])

    it "singleton list, 0 offset" $
        rotate 0 [1] `shouldBe` [1]
    it "singleton list, positive offset" $
        rotate 2 [1] `shouldBe` [1]
    it "singleton list, negative offset" $
        rotate (-2) [1] `shouldBe` [1]

    let xs = [1..6]
    it "finite list, 0 offset" $
        rotate 0 xs `shouldBe` xs
    it "finite list, positive offset" $
        rotate 2 xs `shouldBe` [3, 4, 5, 6, 1, 2]
    it "finite list, negative offset" $
        rotate (-2) xs `shouldBe` [5, 6, 1, 2, 3, 4]

    it "finite list, positive offset equals length" $
        rotate 6 xs `shouldBe` xs
    it "finite list, negative offset equals negative length" $
        rotate (-6) xs `shouldBe` xs

    it "finite list, positive offset greater than length" $
        rotate 10 xs `shouldBe` [5, 6, 1, 2, 3, 4]
    it "finite list, negative offset greater than length" $
        rotate (-10) xs `shouldBe` [3, 4, 5, 6, 1, 2]

    it "infinite list, 0 offset" $
        take 10 (rotate 0 [1..]) `shouldBe` [1..10]
    it "ininite list, positive offset" $
        take 10 (rotate 10 [1..]) `shouldBe` [11..20]

    let bigOffset = 3*10^8
    it "finite list, huge positive offset" $
        rotate bigOffset xs `shouldBe` rotate (bigOffset `mod` length xs) xs
    it "finite list, huge negative offset" $
        rotate (-bigOffset) xs
            `shouldBe` rotate ((-bigOffset) `mod` length xs) xs

rotationsSpec :: Spec
rotationsSpec = do
    it "empty list" $ rotations ([] :: [()]) `shouldBe` [[]]
    it "finite list" $ rotations [1, 2, 3, 4] `shouldBe`
        [[1, 2, 3, 4], [2, 3, 4, 1], [3, 4, 1, 2], [4, 1, 2, 3]]
    it "infinite list" $ take 4 (map (take 4) $ rotations [1..]) `shouldBe`
        [[1, 2, 3, 4], [2, 3, 4, 5], [3, 4, 5, 6], [4, 5, 6, 7]]

combinationsOfSizeSpec :: Spec
combinationsOfSizeSpec = do
    it "size < 0" $ combinationsOfSize (-1) undefined `shouldBe` ([] :: [[()]])
    it "size == 0" $ combinationsOfSize 0 undefined `shouldBe` ([[]] :: [[()]])
    it "empty list" $ do
        combinationsOfSize 1 [] `shouldBe` ([] :: [[()]])
        combinationsOfSize 2 [] `shouldBe` ([] :: [[()]])
    it "singleton list" $ do
        combinationsOfSize 1 [1] `shouldBe` [[1]]
        combinationsOfSize 2 [1] `shouldBe` []
    it "finite list" $ do
        combinationsOfSize 1 [1, 2, 3, 4] `shouldBe` [[1], [2], [3], [4]]
        combinationsOfSize 2 [1, 2, 3, 4]
            `shouldBe` [[1, 2], [1, 3], [1, 4], [2, 3], [2, 4], [3, 4]]
        combinationsOfSize 3 [1, 2, 3, 4]
            `shouldBe` [[1, 2, 3], [1, 2, 4], [1, 3, 4], [2, 3, 4]]
        combinationsOfSize 4 [1, 2, 3, 4]
            `shouldBe` [[1, 2, 3, 4]]
    it "infinite list" $
        take 5 (combinationsOfSize 3 [1..])
            `shouldBe` [[1, 2, 3], [1, 2, 4], [1, 2, 5], [1, 2, 6], [1, 2, 7]]

permutationsOfSizeSpec :: Spec
permutationsOfSizeSpec = do
    it "size < 0" $ permutationsOfSize (-1) undefined `shouldBe` ([] :: [[()]])
    it "size == 0" $ permutationsOfSize 0 undefined `shouldBe` ([[]] :: [[()]])
    it "empty list" $ do
        permutationsOfSize 1 [] `shouldBe` ([] :: [[()]])
        permutationsOfSize 2 [] `shouldBe` ([] :: [[()]])
    it "singleton list" $ do
        permutationsOfSize 1 [1] `shouldBe` [[1]]
        permutationsOfSize 2 [1] `shouldBe` []

    let naive n = sort . (combinationsOfSize n >=> permutations)
        test n xs = permutationsOfSize n xs `shouldBe` naive n xs
    it "finite list" $
        forM_ [1..5] $ \i -> test i [1, 2, 3, 4]

derangementsSpec :: Spec
derangementsSpec = do
    it "empty list" $ derangements [] `shouldBe` ([[]] :: [[()]])
    it "singleton" $ derangements [1] `shouldBe` []
    it "list of length 2" $ derangements [1, 2] `shouldBe` [[2, 1]]
    it "list of length 4" $ derangements [1, 2, 3, 4] `shouldBe`
        [ [2,1,4,3], [2,3,4,1], [2,4,1,3], [3,1,4,2], [3,4,1,2]
        , [3,4,2,1], [4,1,2,3], [4,3,1,2], [4,3,2,1]]
