{-# LANGUAGE ScopedTypeVariables #-}
module Data.List.DigitSpec (spec) where

import Control.Monad   (forM_)
import Data.Proxy      (Proxy (Proxy))

import Test.Hspec      (Expectation, Spec, describe, it, shouldBe)
import Test.QuickCheck (Gen, Property, arbitrary, elements, forAll, listOf,
                        suchThat, (===))

import Data.List.Digit (fromDigits, toDigits)

spec :: Spec
spec = do
    describe "fromDigits" fromDigitsSpec
    describe "toDigits" toDigitsSpec

digits :: (Integral a) => [a]
digits = [0..9]

fromDigitsSpec :: Spec
fromDigitsSpec = do
    it "empty list, output type Int" $
        fromDigits [] `shouldBe` (0 :: Int)
    it "empty list, output type Integer" $
        fromDigits [] `shouldBe` (0 :: Integer)

    let testDigits :: (Integral a, Show a) => [a] -> Expectation
        testDigits ds = forM_ ds $ \x ->
            fromDigits [fromIntegral x] `shouldBe` x
    it "single-digit inputs, output type Int" $
        testDigits (digits :: [Int])
    it "single-digit inputs, output type Integer" $
        testDigits (digits :: [Integer])

    let naive :: (Integral a, Show a, Read a) => [Int] -> a
        naive = read . concatMap show . (0:)

        test :: forall a. (Integral a, Show a, Read a)
             => Proxy a -> [Int] -> Property
        test _ xs =
            toInteger (fromDigits xs :: a) === (naive xs :: Integer)

        digitListGen :: Gen [Int]
        digitListGen = listOf (elements digits)

        -- Lists that will not cause Int overflow
        smallListGen :: Gen [Int]
        smallListGen = digitListGen `suchThat` (not . overflow)
          where
            overflow xs = (naive xs :: Integer) > toInteger (maxBound :: Int)
    it "arbitrary inputs, output type Int" $
        forAll smallListGen (test (Proxy :: Proxy Int))
    it "arbitrary inputs, output type Integer" $
        forAll digitListGen (test (Proxy :: Proxy Integer))

toDigitsSpec :: Spec
toDigitsSpec = do
    let testDigits :: (Integral a) => [a] -> Expectation
        testDigits ds = forM_ ds $ \x ->
            toDigits x `shouldBe` [fromIntegral x]
    it "single-digit Ints"     $ testDigits (digits :: [Int])
    it "single-digit Integers" $ testDigits (digits :: [Integer])
    it "double digits"         $ toDigits 34 `shouldBe` [3, 4]

    let naive :: (Integral a, Show a) => a -> [Int]
        naive = map (read . (:[])) . show

        test :: (Integral a, Show a) => a -> Property
        test x = toDigits x === naive x
    it "arbitrary length Int" $
        forAll (arbitrary `suchThat` (>= 0) :: Gen Int) test
    it "arbitrary length Integer" $
        forAll (arbitrary `suchThat` (>= 0) :: Gen Integer) test
