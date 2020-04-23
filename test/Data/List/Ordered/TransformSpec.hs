{-# LANGUAGE TupleSections #-}
module Data.List.Ordered.TransformSpec (spec) where

import Test.Hspec            (Spec, describe, it, shouldBe, Expectation, hspec)
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck (listOf, arbitrary, forAll, forAllShow, Gen, (===), Property, generate, infiniteList, quickCheck, Testable, choose, infiniteListOf)
import qualified Test.QuickCheck as QC

import Data.List.Ordered.Transform (merge, mergeBy)
import Data.Ord (comparing, Down(Down))

import Data.List (sort, sortBy)

-- TODO: Remove if unnecessary
numTests :: Int
numTests = 100

maxListLength :: Int
maxListLength = 100

spec :: Spec
spec = modifyMaxSuccess (const numTests) $ do
       describe "merge" mergeSpec
       describe "mergeBy" mergeBySpec

unexp :: Expectation
unexp = undefined

mergeSpec :: Spec
mergeSpec = do
  it "both empty" $
    merge [] [] `shouldBe` ([] :: [Int])
  it "left empty" $
    merge [] [1, 2, 3] `shouldBe` ([1, 2, 3] :: [Int])
  it "right empty" $
    merge [1, 2, 3] [] `shouldBe` ([1, 2, 3] :: [Int])
  it "arbitrary finite lists" $ do
    forAll (sortedGen Finite) $ \xs ->
      forAll (sortedGen Finite) $ \ys -> 
        merge xs ys === sort (xs ++ ys)
  it "arbitrary infinite lists" $ do
    forAllInfinite (pairOf $ sortedGen Infinite) $ \(xs, ys) ->
        (trunc $ merge xs ys)
          === (trunc $ sort (trunc xs ++ trunc ys))

mergeBySpec :: Spec
mergeBySpec = do
  it "both empty" $
    mergeBy undefined [] [] `shouldBe` ([] :: [Int])
  it "left empty" $
    mergeBy undefined [] [1, 2, 3] `shouldBe` ([1, 2, 3] :: [Int])
  it "right empty" $
    mergeBy undefined [1, 2, 3] [] `shouldBe` ([1, 2, 3] :: [Int])
  it "arbitrary finite lists" $ do
    let gen = map negate <$> sortedGen Finite
    forAll gen $ \xs ->
      forAll gen $ \ys -> 
        mergeBy (comparing Down) xs ys
          === sortBy (comparing Down) (xs ++ ys)
  it "finite lists with lots of repeats" $ do
    let gen = map negate <$> repeatedSortedGen Finite
    forAll gen $ \xs ->
      forAll gen $ \ys -> 
        mergeBy (comparing Down) xs ys
          === sortBy (comparing Down) (xs ++ ys)
  it "arbitrary infinite lists" $ do
    let gen = map negate <$> sortedGen Infinite
    forAllInfinite (pairOf gen)  $ \(xs, ys) ->
      let test = trunc $ mergeBy (comparing Down) xs ys
          expected = trunc $ sortBy (comparing Down) (trunc xs ++ trunc ys)
       in test === expected
  it "infinite lists with lots of repeats" $ do
    let gen = map negate <$> repeatedSortedGen Infinite
    forAllInfinite (pairOf gen)  $ \(xs, ys) ->
      let test = trunc $ mergeBy (comparing Down) xs ys
          expected = trunc $ sortBy (comparing Down) (trunc xs ++ trunc ys)
       in test === expected
  -- TODO: Test for lots of collisions
  it "left side is preferred on ties" $ do
    let xs = map ("x",) [1, 3, 4, 4, 5] :: [(String, Int)]
    let ys = map ("y",) [1, 3, 3, 4, 5]
    mergeBy (comparing snd) xs ys `shouldBe`
      [("x", 1), ("y", 1), ("x", 3), ("y", 3), ("y", 3),
       ("x", 4), ("x", 4), ("y", 4), ("x", 5), ("y", 5)]

-- Utilities
data Finiteness = Finite | Infinite

sortedGen :: Finiteness -> Gen [Int]
sortedGen finiteness =
  let unsorted = case finiteness of
                   Finite -> listOf arbitrary
                   Infinite -> infiniteList
   in scanl1 (+) <$> filter (>= 0) <$> unsorted

repeatedSortedGen :: Finiteness -> Gen [Int]
repeatedSortedGen f = do
  xs <- sortedGen f
  rs <- infiniteListOf (choose (1, 3))
  return $ concat $ zipWith replicate rs xs

forAllInfinite :: (Show a, Testable prop) => Gen ([a], [a]) -> (([a], [a]) -> prop) -> Property
forAllInfinite gen = forAllShow gen show'
  where show' (xs, ys) = show (trunc xs, trunc ys)

pairOf :: Gen a -> Gen (a, a)
pairOf gen = (,) <$> gen <*> gen

trunc :: [a] -> [a]
trunc = take maxListLength
