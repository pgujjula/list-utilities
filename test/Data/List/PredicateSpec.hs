module Data.List.PredicateSpec where

import Data.Function (on)

import Test.Hspec            (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess)

import Data.List.Predicate   (allEqual, allEqualBy)

-- TODO: Remove if not needed
numTests :: Int
numTests = 1000

spec :: Spec
spec = modifyMaxSuccess (const numTests) $ do
       describe "allEqual" allEqualSpec
       describe "allEqualBy" allEqualBySpec

allEqualSpec :: Spec
allEqualSpec = do
  it "empty list" $ 
    allEqual ([] :: [Int]) `shouldBe` True
  it "singleton" $
    allEqual ([3] :: [Int]) `shouldBe` True
  it "repeated element, finite list" $
    allEqual (replicate 10 (3 :: Int)) `shouldBe` True
  it "single unequal element, finite list" $
    (allEqual $ (replicate 10 (3 :: Int)) ++ [4] ++ (replicate 10 3)) `shouldBe` False
  it "single unequal element, infinite list" $ 
    (allEqual $ (replicate 10 (3 :: Int)) ++ [4] ++ (repeat 3)) `shouldBe` False

allEqualBySpec :: Spec
allEqualBySpec = do
  it "empty list" $ 
    allEqualBy undefined ([] :: [Int]) `shouldBe` True
  it "singleton" $
    allEqualBy undefined ([3] :: [Int]) `shouldBe` True

  let eq = (==) `on` (`rem` (10 :: Int))
  it "repeated element, finite list" $
    allEqualBy eq [3, 13, 23 :: Int] `shouldBe` True
  it "single unequal element, finite list" $ do
    let xs = [3, 13, 23, 34, 43]
    allEqualBy eq xs `shouldBe` False
  it "single unequal element, infinite list" $ do
    let xs = [3, 13, 23, 34] ++ [43, 53..]
    allEqualBy eq xs `shouldBe` False
