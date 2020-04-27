{-| Module      : Data.List.Transform
    Description : Transform lists to other lists.
    Copyright   : (c) Preetham Gujjula, 2020
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

Transform lists to other lists.
-}
module Data.List.Transform (
    -- * Filters
      takeEvery
    , takeUntil
    , dropUntil

    -- * Grouping elements
    , group
    , groupBy
    , groupAdj
    , groupAdjBy

    -- * Deleting duplicates
    , deleteDups
    , deleteDupsBy
    , deleteAdjDups
    , deleteAdjDupsBy

    -- * Miscellaneous
    , rotate
    ) where

import Control.Monad (guard)
import Data.List     (sort, sortBy, uncons)
import Data.Maybe    (fromMaybe)

{-| @takeEvery n xs@ is a list of every @n@th element of @xs@.

    __Precondition:__ @n@ must be positive.

    >>> takeEvery 3 [1..10]
    [3, 6, 9]
    >>> takeEvery 1 [1..10] == [1..10]
    True
-}
takeEvery :: Int -> [a] -> [a]
takeEvery step xs = compute validated
  where compute ys = case drop (step - 1) ys of
                         []    -> []
                         y:ys' -> y : compute ys'
        validated
          | step > 0  = xs
          | otherwise = error $ "Data.List.Transform.takeEvery: Step parameter "
                             ++ "must be positive."

{-| Take a list until a predicate is satisfied, and include the element
    satisfying the predicate.

    >>> takeUntil (== 5) [1..]
    [1, 2, 3, 4, 5]
    >>> takeUntil (== 7) [3, 2, 1]
    [3, 2, 1]
    >>> takeUntil undefined []
    []

    Note that @takeUntil@ on a nonempty list must always yield the first
    element, and the implementation is lazy enough to take advantage of this
    fact.

    >>> head (takeUntil undefined [1..])
    1
-}
takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ []     = []
takeUntil _ [x]    = [x]
takeUntil f (x:xs) = x : (if f x then [] else takeUntil f xs)

{-| Drop a list until a predicate is satisfied, and include the element
    satisfying the predicate.

    >>> dropUntil (== 5) [1..10]
    [5, 6, 7, 8, 9, 10]
    >>> dropUntil (< 0) [1, 2, 3]
    []
    >>> dropUntil undefined []
    []
-}
dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil _ []     = []
dropUntil f (x:xs) = if f x then x:xs else dropUntil f xs

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

{-| Rotate a list by an offset. Positive offset is left rotation, negative is
    right.

    >>> rotate 2 [1..6]
    [3, 4, 5, 6, 1, 2]
    >>> rotate (-2) [1..6]
    [5, 6, 1, 2, 3, 4]
    >>> rotate 0 [1..6]
    [1, 2, 3, 4, 5, 6]

    @rotate@ also works efficiently even if the offset is very large, by
    reducing it modulo the length of the list.

    >>> rotate (60000000000000 + 2) [1..6]
    [3, 4, 5, 6, 1, 2]

    @rotate@ can do zero- and left- shifts on infinite lists, but right-shifts
    will diverge.

    >>> take 6 $ rotate 2 [1..]  -- This works
    [3, 4, 5, 6, 7, 8]
    >>> take 6 $ rotate (-2) [1..]  -- This diverges
-}
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zs ++ ys
  where
    (ys, zs) = splitAt nModLength xs
    nModLength | n < 0     = n `mod` length xs
               | otherwise = n `rem` lengthTo n xs

    -- The length of an list, up to a maximum mx. The length of any list
    -- with length longer than m is reported as mx + 1.
    lengthTo :: Int -> [a] -> Int
    lengthTo mx = sum . take (mx + 1) . map (const 1)
