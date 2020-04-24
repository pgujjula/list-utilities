{-| Module      : Data.List.Transform
    Description : Transform lists to other lists.
    Copyright   : (c) Preetham Gujjula, 2020
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

    Transform lists to other lists.
-}
module Data.List.Transform
  ( takeEvery
  , takeUntil
  , dropUntil

  , group
  , groupBy
  , groupAdj
  , groupAdjBy

  , deleteDups
  , deleteDupsBy
  , deleteAdjDups
  , deleteAdjDupsBy

  , rotate
  ) where

import Control.Monad (guard)
import Data.List     (sort, sortBy, uncons)
import Data.Maybe    (fromMaybe)

-- TODO: Consider moving 3 functions below to Data.List.Filter
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

{-| Take a list until a predicate is satisfied, and include the element
    satisfying the predicate. @takeUntil@ is as lazy as possible, which means it
    has interesting semantics:
      * takeUntil _|_ [] == [] (this is unsurprising)
      * takeUntil _|_ (x:[]) == x:[]
      * takeUntil _|_ (x1:x2:xs) = x1:_|_
    
    >>> takeUntil (== 5) [1..]
    [1, 2, 3, 4, 5]
    >>> takeUntil undefined [1]
    [1]
    >>> head (takeUntil undefined [1..])
    1
-}
takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil _ [x] = [x]
takeUntil f (x:xs) = x:(if f x then [] else takeUntil f xs)

{-| Drop a list until a predicate is satisfied, and include the element
    satisfying the predicate.
    
    >>> dropUntil (== 5) [1..10]
    [5, 6, 7, 8, 9, 10]
-}
dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil _ [] = []
dropUntil f (x:xs) = if f x then (x:xs) else dropUntil f xs

{-| @group xs@ groups elements of xs that are equal. The groups are returned in
    a sorted order, so a group of a smaller element appears before a group of a
    larger one.

    >>> group [1, 3, 2, 3, 2, 3]
    [[1], [2, 2], [3, 3, 3]]
    >>> group []
    []
-}
group :: (Ord a) => [a] -> [[a]]
group = groupAdj . sort

{-| Like @group@, but with a custom comparison test. -}
groupBy :: (a -> a -> Ordering) -> [a] -> [[a]]
groupBy cmp = groupAdjBy eq . sortBy cmp
  where eq a b = cmp a b == EQ

{-| @groupAdjacent xs@ groups adjacent elements of xs that are equal. It works
    with infinite lists as well.

    >>> groupAdj [1, 3, 3, 3, 2, 2]
    [[1], [3, 3, 3], [2, 2]]
    >>> take 4 $ groupAdj $ concatMap (\x -> take x $ repeat x) [1..]
    [[1], [2, 2], [3, 3, 3], [4, 4, 4, 4]]
    >>> groupAdj []
    []
-}
groupAdj :: (Eq a) => [a] -> [[a]]
groupAdj = groupAdjBy (==)

{-| Like @groupAdjacent@, but with a custom equality test. -}
groupAdjBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupAdjBy eq = foldr f []
  where
    f x yss = (x:zs):zss
      where
        (zs, zss) = fromMaybe ([], yss) $ do
          (ys, yss') <- uncons yss
          guard (x `eq` head ys)
          return (ys, yss')

-- TODO: Implement.
deleteDups :: (Ord a) => [a] -> [a]
deleteDups = deleteAdjDups . sort

deleteDupsBy :: (a -> a -> Ordering) -> [a] -> [a]
deleteDupsBy cmp = deleteAdjDupsBy eq . sortBy cmp
  where eq a b = cmp a b == EQ

deleteAdjDups :: (Eq a) => [a] -> [a]
deleteAdjDups = deleteAdjDupsBy (==)

deleteAdjDupsBy :: (a -> a -> Bool) -> [a] -> [a]
deleteAdjDupsBy _ [] = []
deleteAdjDupsBy eq (xs@(x:_)) = x:(map fst $ filter (not . (uncurry eq)) $ zip (tail xs) xs)

{-| Rotate a list by an offset. Positive offset is left rotation, negative is
    right. Zero- and left-rotation work with infinite lists. Also works if the
    offset is greater than the length of the list.

    >>> rotate 2 [1..6]
    [5, 6, 1, 2, 3, 4]
    >>> rotate (-2) [1..6]
    [3, 4, 5, 6, 1, 2]
    >>> rotate 0 [1..6]
    [1, 2, 3, 4, 5, 6]
    >>> take 6 $ rotate (-2) [1..]
    [3, 4, 5, 6, 7, 8]
    >>> rotate 5 [1, 2, 3]
    [2, 3, 1]
-}
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = 
  let (ys, zs) = splitAt nModLength xs
   in zs ++ ys
  where nModLength
          | n < 0     = n `mod` length xs
          | otherwise = n `rem` lengthTo n xs

        -- The length of an list, up to a maximum mx. The length of any list
        -- with length longer than m is reported as mx + 1.
        lengthTo :: Int -> [a] -> Int
        lengthTo mx = sum . take (mx + 1) . zipWith const (repeat 1)
