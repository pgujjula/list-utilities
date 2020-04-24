{-# LANGUAGE TupleSections #-}
module Data.List.Ordered.TransformSpec (spec) where

import Test.Hspec            (Spec, describe, it, shouldBe, Expectation, hspec, shouldStartWith)
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck (listOf, arbitrary, forAll, forAllShow, Gen, (===), Property,
                        generate, infiniteList, quickCheck, Testable, choose, infiniteListOf, getSize)
import qualified Test.QuickCheck as QC

import Data.List.Ordered.Transform (merge, mergeBy, diff, diffBy, intersect, intersectBy, union, unionBy)
import Data.List ((\\))
import qualified Data.List as List (intersect, union)
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
       describe "diff" diffSpec
       describe "diffBy" diffBySpec
       describe "intersect" intersectSpec
       describe "intersectBy" intersectBySpec
       describe "union" unionSpec 
       describe "unionBy" unionBySpec
       describe "test functions together" togetherSpec

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

diffSpec :: Spec
diffSpec = do
  it "both empty" $
    diff [] [] `shouldBe` ([] :: [Int])
  it "left empty" $
    diff [] undefined `shouldBe` ([] :: [Int])
  it "right empty" $
    diff [1, 2, 3] [] `shouldBe` ([1, 2, 3] :: [Int])
  it "arbitrary finite lists" $
    forAll (pairOf (sortedGen2 Finite)) $ \(xs, ys) ->
      (diff xs ys) `shouldBe` (xs \\ ys)
  -- too hard to construct arbitrary infinite lists test.
  -- simplest way to determine a prefix of the difference of two infinite lists
  -- is to construct the diff algorithm itself. So we settle for a hand-made unit test.
  it "infinite lists" $
    let xs = [1..]
        ys = map (^2) [1..]
        ds = diff xs ys
        square x = any (\t -> t^2 == x) [1..x]
     in filter (not . square) xs `shouldStartWith` (take maxListLength $ diff xs ys)

diffBySpec :: Spec
diffBySpec = do
  it "both empty" $
    diffBy undefined [] [] `shouldBe` ([] :: [Int])
  it "left empty" $
    diffBy undefined [] undefined `shouldBe` ([] :: [Int])
  it "right empty" $
    diffBy undefined [1, 2, 3] [] `shouldBe` ([1, 2, 3] :: [Int])
  -- The functionality testing is done in diffSpec. This is just a sanity check
  it "finite list" $
    diffBy (comparing Down) [4, 3, 3, 2, 1] [3, 2, 1] `shouldBe` [4, 3]

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

intersectSpec :: Spec
intersectSpec = do
  it "both empty" $
    intersect [] [] `shouldBe` ([] :: [Int])
  it "left empty" $
    intersect [] [1, 2, 3] `shouldBe` ([] :: [Int])
  it "right empty" $
    intersect [1, 2, 3] [] `shouldBe` ([] :: [Int])

  -- since the Data.List implementation of intersect doesn't have the multiset
  -- semantics of this impelementation, we can't use it as a reference for 
  -- auto-generated QuickCheck tests. Implementing a "naive" intersect is also
  -- difficult. So we settle for hand-written unit tests.
  it "finite list" $
    intersect [1, 2, 3, 3, 3, 4, 5, 6] [2, 3, 3, 5, 7] `shouldBe`
      [2, 3, 3, 5]

intersectBySpec :: Spec
intersectBySpec = do
  it "both empty" $
    intersectBy undefined [] [] `shouldBe` ([] :: [Int])
  it "left empty" $
    intersectBy undefined [] [1, 2, 3] `shouldBe` ([] :: [Int])
  it "right empty" $
    intersectBy undefined [1, 2, 3] [] `shouldBe` ([] :: [Int])
  it "finite list" $
    intersectBy (comparing Down) [3, 2, 1] [3, 1, 0] `shouldBe` [3, 1]
  it "left side preferred" $ do
    let xs = map ("x",) [1, 2, 3, 4]
    let ys = map ("y",) [1, 2, 3, 5]
    intersectBy (comparing snd) xs ys `shouldBe` map ("x",) [1, 2, 3]

unionSpec :: Spec
unionSpec = do
  it "both empty" $
    union [] [] `shouldBe` ([] :: [Int])
  it "left empty" $
    union [] [1, 2, 3] `shouldBe` ([1, 2, 3] :: [Int])
  it "right empty" $
    union [1, 2, 3] [] `shouldBe` ([1, 2, 3] :: [Int])
  it "finite list" $
    union [1, 3, 3, 4, 5] [2, 3, 5, 7] `shouldBe` [1, 2, 3, 3, 4, 5, 7]

unionBySpec :: Spec
unionBySpec = do
  it "both empty" $
    unionBy undefined [] [] `shouldBe` ([] :: [Int])
  it "left empty" $
    unionBy undefined [] [1, 2, 3] `shouldBe` ([1, 2, 3] :: [Int])
  it "right empty" $
    unionBy undefined [1, 2, 3] [] `shouldBe` ([1, 2, 3] :: [Int])
  it "finite list" $
    unionBy (comparing Down) [3, 2, 1] [3, 1, 0] `shouldBe` [3, 2, 1, 0]
  it "left side preferred" $ do
    let xs = map ("x",) [1, 2, 3, 4]
    let ys = map ("y",) [1, 2, 3, 5]
    unionBy (comparing snd) xs ys `shouldBe` 
      [("x", 1), ("x", 2), ("x", 3), ("x", 4), ("y", 5)]

togetherSpec :: Spec
togetherSpec = do
  it "x ∪ y == (x ∩ y) + (x - y) + (y - x)" $ do
    forAll (pairOf (repeatedSortedGen Finite)) $ \(xs, ys) ->
      (union xs ys) === (sort $ (intersect xs ys) ++ (diff xs ys) ++ (diff ys xs))

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

-- generators
-- finite sorted list
sortedGen2 :: Finiteness -> Gen [Int]
sortedGen2 finiteness = do
  rs <- infiniteListOf (choose (1, 4))
  xs <- case finiteness of
          Finite -> do s <- getSize
                       len <- choose (1, s)
                       return [1..len]
          Infinite -> return [1..]
  return $ concat $ zipWith replicate rs xs
