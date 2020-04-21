module Data.List.TransformSpec (spec) where

import Test.Hspec            (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess)

import Data.List.Transform   (takeEvery)

numTests :: Int
numTests = 1000

spec :: Spec
spec = modifyMaxSuccess (const numTests) $ do
       describe "takeEvery" takeEverySpec

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
