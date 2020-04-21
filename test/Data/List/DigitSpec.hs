{-# LANGUAGE ScopedTypeVariables #-}
module Data.List.DigitSpec (spec) where

import Control.Monad         (forM_)
import Data.Proxy            (Proxy (Proxy))

import System.Random         (Random)
import Test.Hspec            (Expectation, Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck       (Gen, Property, choose, elements, forAll, listOf,
                              oneof, resize, sized, suchThat, (===))

import Data.List.Digit       (fromDigits, numDigits, sumDigits, toDigits)

numTests :: Int
numTests = 1000

maxNumDigits :: Int
maxNumDigits = 50

spec :: Spec
spec = modifyMaxSuccess (const numTests) $ do
       describe "numDigits" numDigitsSpec
       describe "sumDigits" sumDigitsSpec
       describe "toDigits" toDigitsSpec
       describe "fromDigits" fromDigitsSpec

numDigitsSpec :: Spec
numDigitsSpec = do
  let testDigits :: (Integral a) => [a] -> Expectation
      testDigits ds = forM_ ds $ \x -> numDigits x `shouldBe` 1
   in do
      it "single-digit Ints"
        $ testDigits (signedDigits :: [Int])
      it "single-digit Integers"
        $ testDigits (signedDigits :: [Integer])

  let naive :: (Integral a, Show a) => a -> Int
      naive = length . show . abs

      testArbitrary :: (Integral a, Show a) => a -> Property
      testArbitrary x = numDigits x === naive x
   in do
      it "arbitrary-length Ints"
        $ forAll uniformLengthIntGen testArbitrary
      it "arbitrary-length Integers"
        $ forAll uniformLengthIntegerGen testArbitrary

sumDigitsSpec :: Spec
sumDigitsSpec = do
  let testDigits :: (Integral a) => [a] -> Expectation
      testDigits ds = forM_ ds $ \x -> sumDigits x `shouldBe` fromIntegral (abs x)
   in do
      it "single-digit Ints"
        $ testDigits (signedDigits :: [Int])
      it "single-digit Integers"
        $ testDigits (signedDigits :: [Integer])

  let naive :: (Integral a, Show a) => a -> Int
      naive = sum . map (read . (:[])) . show . abs

      testArbitrary :: (Integral a, Show a) => a -> Property
      testArbitrary x = sumDigits x === naive x
   in do
      it "arbitrary-length Ints"
        $ forAll uniformLengthIntGen testArbitrary
      it "arbitrary-length Integers"
        $ forAll uniformLengthIntegerGen testArbitrary

toDigitsSpec :: Spec
toDigitsSpec = do
  let testDigits :: (Integral a) => [a] -> Expectation
      testDigits ds = forM_ ds $ \x ->
        toDigits x `shouldBe` [fromIntegral x :: Int]

   in do
      it "single-digit Ints"
        $ testDigits (digits :: [Int])
      it "single-digit Integers"
        $ testDigits (digits :: [Integer])

  let naive :: (Integral a, Show a) => a -> [Int]
      naive = map (read . (:[])) . show

      testArbitrary :: (Integral a, Show a) => a -> Property
      testArbitrary x = toDigits x === naive x
   in do
      it "arbitrary length Int"
        $ forAll (uniformLengthIntGen `suchThat` (>= 0)) testArbitrary
      it "arbitrary length Integer"
        $ forAll (uniformLengthIntegerGen `suchThat` (>= 0)) testArbitrary

fromDigitsSpec :: Spec
fromDigitsSpec = do
  it "empty list, output type Int"
    $ fromDigits [] `shouldBe` (0 :: Int)
  it "empty list, output type Integer"
    $ fromDigits [] `shouldBe` (0 :: Integer)

  let testDigits :: (Integral a, Show a) => [a] -> Expectation
      testDigits ds = forM_ ds $ \x ->
        fromDigits [fromIntegral x :: Int] `shouldBe` x

   in do
      it "single-digit inputs, output type Int"
        $ testDigits (digits :: [Int])
      it "single-digit inputs, output type Integer"
        $ testDigits (digits :: [Integer])

  let naive :: (Integral a, Show a, Read a) => [Int] -> a
      naive = read . concatMap show . (0:)

      testArbitrary :: forall a. (Integral a, Show a, Read a)
                    => Proxy a -> [Int] -> Property
      testArbitrary _ xs = toInteger (fromDigits xs :: a)
                       === (naive xs :: Integer)

      digitListGen :: Gen [Int]
      digitListGen = resize maxNumDigits $ listOf $ elements digits

      -- Lists that will not cause Int overflow
      smallListGen :: Gen [Int]
      smallListGen = digitListGen `suchThat` (not . overflow)
        where overflow xs = (naive xs :: Integer) > toInteger (maxBound :: Int)
   in do
      it "arbitrary inputs, output type Int"
        $ forAll smallListGen (testArbitrary (Proxy :: Proxy Int))
      it "arbitrary inputs, output type Integer"
        $ forAll digitListGen (testArbitrary (Proxy :: Proxy Integer))

{-
   Shared test data, generators
-}
digits :: (Integral a) => [a]
digits = [0..9]

signedDigits :: (Integral a) => [a]
signedDigits = [-9..9]

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
  where maxIntLength = length (show (maxBound :: Int)) - 1

uniformLengthIntegerGen :: Gen Integer
uniformLengthIntegerGen = resize maxNumDigits uniformLengthIntegralGen
