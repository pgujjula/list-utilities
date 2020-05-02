module Data.List.FilterSpec where

import Data.List.Filter (takeEvery, dropEvery, takeUntil, dropUntil)

import Test.Hspec (Spec, shouldBe, describe, it)

empty :: [Integer]
empty = []

singletonUndef :: [Integer]
singletonUndef = [undefined]

spec :: Spec
spec = do
    describe "takeEvery" takeEverySpec
    describe "dropEvery" dropEverySpec
    describe "takeUntil" takeUntilSpec
    describe "dropUntil" dropUntilSpec

takeEverySpec :: Spec
takeEverySpec = do
    it "n = 1, empty list" $
        takeEvery 1 empty `shouldBe` empty
    it "n = 1, finite list" $
        takeEvery 1 [1..10] `shouldBe` [1..10]
    it "n = 1, prefix of infinite list" $
        take 10 (takeEvery 1 [1..]) `shouldBe` take 10 [1..10]

    it "n = 3, empty list" $
        takeEvery 3 empty `shouldBe` empty
    it "n = 3, finite list" $
        takeEvery 3 [1..10] `shouldBe` [3, 6, 9]
    it "n = 3, prefix of infinite list" $
        take 10 (takeEvery 3 [1..]) `shouldBe` take 10 [3, 6..]

dropEverySpec :: Spec
dropEverySpec = do
    it "n = 1, empty list" $ 
        dropEvery 1 empty `shouldBe` empty      
    it "n = 1, finite list" $ 
        dropEvery 1 [1..10] `shouldBe` empty      

    it "n = 3, empty list" $
        dropEvery 3 empty `shouldBe` []
    it "n = 3, finite list" $
        dropEvery 3 [1..10] `shouldBe` [1, 2, 4, 5, 7, 8, 10]
    it "n = 3, prefix of infinite list" $
      take 10 (dropEvery 3 [1..])
          `shouldBe` take 10 (filter (\x -> x `rem` 3 /= 0) [1..])


takeUntilSpec :: Spec
takeUntilSpec = do
    it "empty list, undefined function" $
        takeUntil undefined empty `shouldBe` empty
    it "singleton list, undefined function" $
        length (takeUntil undefined singletonUndef) `shouldBe` 1
    it "finite list, undefined function" $
        head (takeUntil undefined [1, 2, 3]) `shouldBe` 1
    it "finite list" $
        takeUntil (>= 5) [1..10] `shouldBe` [1..5]
    it "finite list, always True" $
        takeUntil (const True) [1..10] `shouldBe` [1]
    it "finite list, always False" $
        takeUntil (const False) [1..10] `shouldBe` [1..10]
    it "infinite list" $
        takeUntil (>= 5) [1..] `shouldBe` [1..5]

dropUntilSpec :: Spec
dropUntilSpec = do
    it "empty list, undefined function" $
        dropUntil undefined empty `shouldBe` empty
    it "finite list" $
        dropUntil (== 5) [1..10] `shouldBe` [5..10]
    it "finite list, always True" $
        dropUntil (const True) [1..10] `shouldBe` [1..10]
    it "finite list, always False" $
        dropUntil (const False) [1..10] `shouldBe` []
    it "infinite list" $
        take 6 (dropUntil (== 5) [1..]) `shouldBe` [5..10]
