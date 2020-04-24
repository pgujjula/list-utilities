module Data.List.Gen (Finiteness(..), sortedGen, repeatedSortedGen, forAllInfinite) where

import Test.QuickCheck (Gen, listOf, arbitrary, infiniteList, choose, infiniteListOf, forAllShow, Property, Testable)

data Finiteness = Finite | Infinite

sortedGen :: Finiteness -> Gen [Int]
sortedGen finiteness =
  let unsorted = case finiteness of
                   Finite -> listOf (choose (1, 10000))
                   Infinite -> infiniteListOf (choose (1, 10000))
   in scanl1 (+) <$> unsorted

repeatedSortedGen :: Finiteness -> Gen [Int]
repeatedSortedGen f = do
  xs <- sortedGen f
  rs <- infiniteListOf (choose (1, 3))
  return $ concat $ zipWith replicate rs xs

forAllInfinite :: (Show a, Testable prop) => Int -> Gen ([a], [a]) -> (([a], [a]) -> prop) -> Property
forAllInfinite maxListLen gen = forAllShow gen show'
  where show' (xs, ys) = show (trunc xs, trunc ys)
        trunc = take maxListLen
