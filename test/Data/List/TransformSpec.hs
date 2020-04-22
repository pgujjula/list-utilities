module Data.List.TransformSpec (spec) where

import Test.Hspec            (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess)

import Data.Function         (on)
import Data.List.Transform   (group, groupAdjacent, groupAdjacentBy, groupBy,
                              takeEvery)
import Data.Ord              (Down (Down), comparing)

numTests :: Int
numTests = 1000

maxListLength :: Int
maxListLength = 1000

spec :: Spec
spec = modifyMaxSuccess (const numTests) $ do
       describe "takeEvery" takeEverySpec
       describe "groupAdjacent" groupAdjacentSpec
       describe "group" groupSpec
       describe "groupBy" groupBySpec
       describe "groupAdjacentBy" groupAdjacentBySpec

takeEverySpec :: Spec
takeEverySpec = do
  it "n = 1, empty list" $
    takeEvery 1 ([] :: [Int]) `shouldBe` []
  it "n = 1, finite list" $ do
    let xs = [1..10] :: [Int]
    takeEvery 1 xs `shouldBe` xs
  it "n = 1, prefix of infinite list" $ do
    let xs = [1..] :: [Int]
    take 10 (takeEvery 1 xs) `shouldBe` take 10 xs

  it "n = 3, empty list" $
    takeEvery 3 ([] :: [Int]) `shouldBe` []
  it "n = 3, finite list" $
    takeEvery 3 [1..10] `shouldBe` ([3, 6, 9] :: [Int])
  it "n = 3, prefix of infinite list" $ do
    let xs = [1..] :: [Int]
    take 10 (takeEvery 3 xs) `shouldBe` take 10 (map (*3) xs)

groupAdjacentSpec :: Spec
groupAdjacentSpec = do
  it "empty list" $
    groupAdjacent ([] :: [Int]) `shouldBe` []
  it "finite list" $
    groupAdjacent ([1, 3, 3, 3, 2, 2] :: [Int]) `shouldBe` [[1], [3, 3, 3], [2, 2]]
  it "infinite list" $ do
    -- output == [[1], [2, 2], [3, 3, 3]..]
    let output = map (\x -> replicate x x) [1..] :: [[Int]]
    let input = concat output
    let n = floor $ sqrt (fromIntegral maxListLength :: Double)
    take n (groupAdjacent input) `shouldBe` take n output

-- TODO: We can add randomized tests once we add predicates to Data.List.Predicate
-- (allEqual, sorted)
groupSpec :: Spec
groupSpec = do
  it "empty list" $
    group ([] :: [Int]) `shouldBe` []
  it "finite list" $
    group ([1, 3, 2, 3, 2, 3] :: [Int]) `shouldBe` [[1], [2, 2], [3, 3, 3]]

groupBySpec :: Spec
groupBySpec =
  let cmp = comparing Down
   in do
      it "empty list" $
        groupBy cmp ([] :: [Int]) `shouldBe` []
      it "finite list" $
        groupBy cmp ([1, 3, 2, 3, 2, 3] :: [Int]) `shouldBe` [[3, 3, 3], [2, 2], [1]]

groupAdjacentBySpec :: Spec
groupAdjacentBySpec =
  let eq = (==) `on` head
   in do
      it "empty list" $
        groupAdjacentBy eq ([] :: [String]) `shouldBe` []
      it "finite list" $ do
        let input = ["apple", "at", "atom", "banana", "bot", "cat", "curry", "clip"]
        let output = [["apple", "at", "atom"], ["banana", "bot"], ["cat", "curry", "clip"]]
        groupAdjacentBy eq input `shouldBe` output
