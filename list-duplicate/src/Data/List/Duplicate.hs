{-| Module      : Data.List.Duplicate
    Description : Group and delete duplicates from a list.
    Copyright   : (c) Preetham Gujjula, 2020
    License     : BSD3
    Maintainer  : pgujjula+list-utilities@protonmail.com
    Stability   : experimental

Group and delete duplicates from a list.
-}
module Data.List.Duplicate (
    -- * Grouping elements
      group
    , groupBy
    , groupAdj
    , groupAdjBy

    -- * Deleting duplicates
    , deleteDups
    , deleteDupsBy
    , deleteAdjDups
    , deleteAdjDupsBy

    -- * Finding the mode
    , mode
    , modeBy
    ) where

import Control.Monad (guard)
import Data.List     (maximumBy, sort, sortBy, uncons)
import Data.Maybe    (fromMaybe)
import Data.Function ((&))
import Data.Ord      (comparing)

{-| /O(n log(n))./ Group the equal elements of the list together, in sorted
    order.

    >>> group [1, 3, 2, 3, 2, 3]
    [[1], [2, 2], [3, 3, 3]]
    >>> group [1]
    [[1]]
    >>> group []
    []
-}
group :: (Ord a) => [a] -> [[a]]
group = groupAdj . sort

{-| /O(n log(n))./ Like 'group', with a custom comparison test. The grouping is
    stable, so if @x@, @y@ are in the same group, and @x@ appears before @y@ in
    the original list, then @x@ appears before @y@ in the group.

    >>> groupBy (comparing head) ["b1", "c1", "a1", "b2", "a2"]
    [["a1", "a2"], ["b1", "b2"], ["c1"]]
-}
groupBy :: (a -> a -> Ordering) -> [a] -> [[a]]
groupBy cmp = groupAdjBy eq . sortBy cmp
  where
    eq a b = cmp a b == EQ

{-| /O(n)./ Group adjacent elements that are equal. Works with infinite lists.
    Useful for grouping equal elements of a sorted list.

    >>> groupAdj [1, 3, 3, 3, 2, 2, 3]
    [[1], [3, 3, 3], [2, 2], [3]]
    >>> take 4 $ groupAdj $ concatMap (\x -> replicate x x) [1..]
    [[1], [2, 2], [3, 3, 3], [4, 4, 4, 4]]
    >>> groupAdj []
    []
    >>> groupAdj [1]
    [[1]]
-}
groupAdj :: (Eq a) => [a] -> [[a]]
groupAdj = groupAdjBy (==)

{-| /O(n)./ Like 'groupAdj', with a custom equality test.

    >>> groupAdjBy ((==) `on` head) ["a1", "a2", "b1", "c1", "a3", "a4"]
    [["a1", "a2"], ["b1"], ["c1"], ["a3", "a4"]]
-}
groupAdjBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupAdjBy eq = foldr f []
  where
    f x yss = (x:zs):zss
      where
        (zs, zss) = fromMaybe ([], yss) $ do
            (ys, yss') <- uncons yss
            guard (x `eq` head ys)
            return (ys, yss')

{-| /O(n log(n))./ Delete duplicates from the list. Output is in sorted order.

    >>> deleteDups [3, 1, 1, 2, 1, 3]
    [1, 2, 3]
-}
deleteDups :: (Ord a) => [a] -> [a]
deleteDups = deleteAdjDups . sort

{-| /O(n log(n))./ Like 'deleteDups', with a custom comparison test. First
    appearances are kept.

    >>> deleteDupsBy (comparing head) ["a1", "c1", "d1", "a2", "b1"]
    ["a1", "b1", "c1", "d1"]
-}
deleteDupsBy :: (a -> a -> Ordering) -> [a] -> [a]
deleteDupsBy cmp = deleteAdjDupsBy eq . sortBy cmp
  where
    eq a b = cmp a b == EQ

{-| /O(n)./ Delete adjacent duplicates from the list. Works with infinite lists.
    Useful for deleting duplicates from a sorted list. Remaining elements are in
    the same relative order.

    >>> deleteAdjDups [1, 3, 4, 4, 4, 3]
    [1, 3, 4, 3]
-}
deleteAdjDups :: (Eq a) => [a] -> [a]
deleteAdjDups = deleteAdjDupsBy (==)

{-| /O(n)./ Like 'deleteAdjDups', with a custom equality test. First appearances
    are kept.

    >>> deleteAdjDupsBy ((==) `on` head) ["a1", "a2", "b1", "b2", "a3", "a4"]
    ["a1", "b1", "a3]
-}
deleteAdjDupsBy :: (a -> a -> Bool) -> [a] -> [a]
deleteAdjDupsBy _ [] = []
deleteAdjDupsBy eq xs@(x:_) =
    x : map fst (filter (not . uncurry eq) $ zip (tail xs) xs)

mode :: Ord a => [a] -> Maybe a
mode = modeBy compare

modeBy :: (a -> a -> Ordering) -> [a] -> Maybe a
modeBy _ [] = Nothing
modeBy f xs =
  groupBy f xs
  & maximumBy (comparing length)
  & head
  & Just
