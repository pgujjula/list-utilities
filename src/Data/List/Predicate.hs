{-| Module      : Data.List.Predicate
    Description : Predicates on lists.
    Copyright   : (c) Preetham Gujjula, 2020
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

    Predicates on lists.
-}
module Data.List.Predicate
  ( allEqual
  , allEqualBy

  , sorted
  , sortedBy

  , allUnique
  , allUniqueBy
  , allAdjUnique
  , allAdjUniqueBy

  , ascSequential
  , descSequential

  , palindrome
  ) where

import Data.List (sort, sortBy)

{-| Whether all the elements in the list are equal.

    >>> allEqual [1..]
    False
    >>> allEqual [3, 3, 3, 3]
    True
    >>> allEqual []
    True
-}
allEqual :: (Eq a) => [a] -> Bool
allEqual = allEqualBy (==)

{-| Like 'allEqual', with a custom equality test.

    >>> allEqualBy ((==) `on` (`rem` 10)) [3, 13, 23]
    True
    >>> allEqualBy ((==) `on` (`rem` 10)) [3, 13, 24]
    False
    >>> allEqualBy undefined []
    True
    >>> allEqualBy undefined [3]
    True
-}
allEqualBy :: (a -> a -> Bool) -> [a] -> Bool
allEqualBy _  []       = True
allEqualBy eq (x : xs) = all (eq x) xs

{-| Whether the elements are in sorted order.

    >>> sorted [1, 2, 3, 3]
    True
    >>> sorted [1, 2, 3, 2]
    False
    >>> sorted []
    True
    >>> sorted [x]
    True
-}
sorted :: (Ord a) => [a] -> Bool
sorted = sortedBy compare

{-| Like 'sorted', with a custom equality test.

    >>> sortedBy (comparing Down) [3, 2, 1]
    True
    >>> sortedBy (comparing Down) [3, 2, 1, 2]
    False
    >>> sortedBy undefined []
    True
    >>> sortedBy undefined [undefined]
    True
-}
sortedBy :: (a -> a -> Ordering) -> [a] -> Bool
sortedBy _   []  = True
sortedBy _   [_] = True
sortedBy cmp xs  = and $ zipWith (\a b -> cmp a b <= EQ) xs (tail xs)

{-| Whether the elements are all unique.

    >>> allUnique [1, 2, 5, 7]
    True
    >>> allUnique [1, 2, 5, 2]
    False
    >>> allUnique []
    True
    >>> allUnique [1]
    True
-}
allUnique :: (Ord a) => [a] -> Bool
allUnique = allAdjUnique . sort

{-| Like 'allUnique', with a custom comparison test.

    >>> allUniqueBy (comparing head) ["apple", "bow", "cat"]
    True
    >>> allUniqueBy (comparing head) ["apple", "bow", "ant"]
    False
    >>> allUniqueBy undefined []
    True
    >>> allUniqueBy undefined [undefined]
    True
-}
allUniqueBy :: (a -> a -> Ordering) -> [a] -> Bool
allUniqueBy cmp = allAdjUniqueBy eq . sortBy cmp
  where
    eq a b = cmp a b == EQ

{-| Whether all adjacent pairs of elements are different.

    >>> allAdjUnique [4, 2, 3, 2]
    True
    >>> allAdjUnique [4, 2, 2, 3]
    False
    >>> allAdjUnique []
    True
    >>> allAdjUnique [1]
    True
-}
allAdjUnique :: (Eq a) => [a] -> Bool
allAdjUnique = allAdjUniqueBy (==)

{-| Like @allAdjUnique@, with a custom equality test.

    >>> allAdjUniqueBy (comparing head) ["apple", "bow", "cat", "ant"]
    True
    >>> allAdjUniqueBy (comparing head) ["apple", "ant", "bow", "cat"]
    False
    >>> allAdjUniqueBy undefined []
    True
    >>> allAdjUniqueBy undefined [undefined]
    True
-}
allAdjUniqueBy :: (a -> a -> Bool) -> [a] -> Bool
allAdjUniqueBy eq xs = (not . or) $ zipWith eq xs (tail xs)

{-| Whether the list is increasing sequentially.

    >>> ascSequential [1, 2, 3, 4, 5]
    True
    >>> ascSequential [1, 2, 3, 4, 8]
    False
    >>> ascSequential []
    True
    >>> ascSequential [1]
    True
-}
ascSequential :: (Enum a) => [a] -> Bool
ascSequential xs = and $ zipWith (==) xs' [head xs' ..]
  where
    xs' = map fromEnum xs

{-| Whether the list is descending sequentially.

    >>> descSequential [5, 4, 3, 2, 1]
    True
    >>> descSequential [5, 4, 3, 3, 1]
    False
    >>> descSequential []
    True
    >>> descSequential [1]
    True
-}
descSequential :: (Enum a) => [a] -> Bool
descSequential xs = and $ zipWith (==) xs' [x, x - 1 ..]
  where
    xs' = map fromEnum xs
    x   = head xs'

{-| Whether the input is a palindrome, i.e., the same forwards and backwards.

    >>> palindrome "rotor"
    True
    >>> palindrome "rover"
    False
    >>> palindrome "a"
    True
    >>> palindrome ""
    True
-}
palindrome :: (Eq a) => [a] -> Bool
palindrome xs = and $ take (len `div` 2) $ zipWith (==) xs rev
  where
    (rev, len) = reverseLength xs

-- Get the reverse and the length of a list in one pass.
reverseLength :: [a] -> ([a], Int)
reverseLength = reverseLengthWith [] 0
  where
    -- Accumulate the reverse and the length.
    reverseLengthWith :: [a] -> Int -> [a] -> ([a], Int)
    reverseLengthWith ys n [] = (ys, n)
    reverseLengthWith ys n (x : xs) =
        let n' = n + 1
         in seq n' (reverseLengthWith (x : ys) n' xs)
