{-# LANGUAGE ScopedTypeVariables #-}
module Data.List.DigitSpec (spec) where

import Control.Monad         (forM_)

import System.Random         (Random)
import Test.Hspec            (Expectation, Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck       (Gen, Property, choose, forAll, oneof, resize,
                              sized, (===))

import Data.List.Digit       (numDigits, sumDigits)

numTests :: Int
numTests = 1000

maxNumDigits :: Int
maxNumDigits = 50

spec :: Spec
spec = modifyMaxSuccess (const numTests) $ do
       describe "numDigits" numDigitsSpec
       describe "sumDigits" sumDigitsSpec

digits :: (Integral a) => [a]
digits = [-9..9]

-- Generate integers where the length in base 10 is uniformly distributed.
-- Take care to prevent overflow.
uniformLengthIntegralGen :: (Random a, Integral a) => Gen a
uniformLengthIntegralGen = sized $ \n -> oneof $ map genLength [1..n]
  where genLength i = oneof [positive, negative]
          where lower = 10^(i - 1)
                upper = 10^i - 1
                positive = choose (lower, upper)
                negative = choose (-upper, -lower)

uniformLengthIntGen :: Gen Int
uniformLengthIntGen = resize maxIntLength uniformLengthIntegralGen
  where maxIntLength = (length $ show (maxBound :: Int)) - 1

uniformLengthIntegerGen :: Gen Integer
uniformLengthIntegerGen = resize maxNumDigits uniformLengthIntegralGen

numDigitsSpec :: Spec
numDigitsSpec = do
  let testDigits :: (Integral a) => [a] -> Expectation
      testDigits ds = forM_ ds $ \x -> numDigits x `shouldBe` 1
   in do
      it "on single-digit Ints equals 1"
        $ testDigits (digits :: [Int])
      it "on single-digit Integers equals 1"
        $ testDigits (digits :: [Integer])

  let naive :: (Integral a, Show a) => a -> Int
      naive = length . show . abs

      testArbitrary :: (Integral a, Show a) => a -> Property
      testArbitrary x = numDigits x === naive x
   in do
      it "correct on Ints of random length"
        $ forAll uniformLengthIntGen testArbitrary
      it "correct on Integers of random length"
        $ forAll uniformLengthIntegerGen testArbitrary

sumDigitsSpec :: Spec
sumDigitsSpec = do
  let testDigits :: (Integral a) => [a] -> Expectation
      testDigits ds = forM_ ds $ \x -> sumDigits x `shouldBe` (fromIntegral $ abs x)
   in do
      it "on single-digit Ints equals 1"
        $ testDigits (digits :: [Int])
      it "on single-digit Integers equals 1"
        $ testDigits (digits :: [Integer])

  let naive :: (Integral a, Show a) => a -> Int
      naive = sum . map (read . (:[])) . show

      testArbitrary :: (Integral a, Show a) => a -> Property
      testArbitrary x = sumDigits x === naive x
   in do
      it "correct on Ints of random length"
        $ forAll uniformLengthIntGen testArbitrary
      it "correct on Integers of random length"
        $ forAll uniformLengthIntegerGen testArbitrary
