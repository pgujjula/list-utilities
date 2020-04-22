{-|
Module      : Data.List.Digit
Description : Transform lists to other lists.
Copyright   : (c) Preetham Gujjula, 2020
License     : GPL-3
Maintainer  : preetham.gujjula@gmail.com
Stability   : experimental

Transform lists to other lists.
-}
module Data.List.Transform
  ( takeEvery
  , group
  , groupBy
  , groupAdjacent
  , groupAdjacentBy
  ) where

import Control.Monad (guard)
import Data.List     (sort, sortBy, uncons)
import Data.Maybe    (fromMaybe)

-- TODO: Consider moving to Data.List.Filter
{-| @takeEvery n xs@ is a list of every nth element of xs

    __Precondition:__ @n@ must be positive.

    >>> takeEvery 3 [1..10]
    [3, 6, 9]
    >>> (takeEvery 1 [1..10]) == [1..10]
    True
-}
takeEvery :: Int -> [a] -> [a]
takeEvery n xs =
  case drop (n - 1) xs of
    []     -> []
    (y:ys) -> y : takeEvery n ys

{-| @group xs@ groups elements of xs that are equal. The groups are returned in
    a sorted order, so a group of a smaller element appears before a group of a
    larger one.

    >>> group [1, 3, 2, 3, 2, 3]
    [[1], [2, 2], [3, 3, 3]]
    >>> group []
    []
-}
group :: (Ord a) => [a] -> [[a]]
group = groupAdjacent . sort

{-| Like @group@, but with a custom comparison test. -}
groupBy :: (a -> a -> Ordering) -> [a] -> [[a]]
groupBy cmp = groupAdjacentBy eq . sortBy cmp
  where eq a b = cmp a b == EQ

{- @groupAdjacent xs@ groups adjacent elements of xs that are equal. It works
   with infinite lists as well.

   >>> groupAdj [1, 3, 3, 3, 2, 2]
   [[1], [3, 3, 3], [2, 2]]
   >>> take 4 $ groupAdj $ concatMap (\x -> take x $ repeat x) [1..]
   [[1], [2, 2], [3, 3, 3], [4, 4, 4, 4]]
   >>> groupAdj []
   []
-}
groupAdjacent :: (Eq a) => [a] -> [[a]]
groupAdjacent = groupAdjacentBy (==)

{-| Like @groupAdjacent@, but with a custom equality test. -}
groupAdjacentBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupAdjacentBy eq = foldr f []
  where
    f x yss = (x:zs):zss
      where
        (zs, zss) = fromMaybe ([], yss) $ do
          (ys, yss') <- uncons yss
          guard (x `eq` head ys)
          return (ys, yss')
