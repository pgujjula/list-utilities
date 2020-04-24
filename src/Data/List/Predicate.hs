{-| Module      : Data.List.Predicate
    Description : Transform lists to other lists.
    Copyright   : (c) Preetham Gujjula, 2020
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

    Transform lists to other lists.
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

{-| Like @allEqual@, with a custom equality test. @allEqual@ is as lazy as
    possible, which means:
      * allEqualBy _|_ [] == True
      * allEqualBy _|_ (_|_:[]) == True
      * allEqualBy _|_ (x1:x2:xs) == undefined
  
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
allEqualBy _ [] = True
allEqualBy eq (x:xs) = all (eq x) xs

{-| Whether the elements are in sorted order. Laziness semantics:
     * sorted _|_:[] == True
    
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

{-| Like @sorted@, with a custom equality test. @sortedBy@ is as lazy as
    possible, which means:
      * sortedBy _|_ [] == True
      * sortedBy _|_ (_|_:[]) == True
      * sortedBy _|_ (x1:x2:xs) == undefined

    >>> sortedBy (comparing Down) [3, 2, 1]
    True
    >>> sorted (comparing Down) [3, 2, 1, 2]
    False
    >>> sorted undefined []
    True
    >>> sorted undefined [undefined]
    True
-}
sortedBy :: (a -> a -> Ordering) -> [a] -> Bool
sortedBy _ [] = True
sortedBy _ [_] = True
sortedBy cmp xs = and $ zipWith (\a b -> cmp a b /= GT) xs (tail xs)

allUnique :: (Ord a) => [a] -> Bool
allUnique = allAdjUnique . sort

{-| Like @allUnique@, with a custom comparison test. -}
allUniqueBy :: (a -> a -> Ordering) -> [a] -> Bool
allUniqueBy cmp = allAdjUniqueBy eq . sortBy cmp
  where eq a b = cmp a b == EQ

allAdjUnique :: (Eq a) => [a] -> Bool
allAdjUnique = allAdjUniqueBy (==)

{-| Like @allAdjUnique@, with a custom equality test. -}
allAdjUniqueBy :: (a -> a -> Bool) -> [a] -> Bool
allAdjUniqueBy eq xs = (not . or) $ zipWith eq xs (tail xs)

{-| Whether the list is increasing sequentially.

    >>> ascSequential [1, 2, 3]
    True
    >>> ascSequential [1, 2, 4]
    False
-}
ascSequential :: (Enum a) => [a] -> Bool
ascSequential xs = and $ zipWith (==) xs' [head xs'..]
  where xs' = map fromEnum xs

{-| Whether the list is descending sequentially.

    >>> descSequential [3, 2, 1]
    True
    >>> descSequential [3, 2, 2]
    False
-}
descSequential :: (Enum a) => [a] -> Bool
descSequential xs = and $ zipWith (==) xs' [x, x-1..]
  where xs' = map fromEnum xs
        x = head xs'

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
palindrome xs = 
  let (rev, len) = reverseLength xs
   in and $ take (len `div` 2) $ zipWith (==) xs rev

reverseLength :: [a] -> ([a], Int)
reverseLength = reverseLengthWith [] 0
  
reverseLengthWith :: [a] -> Int -> [a] -> ([a], Int)
reverseLengthWith ys n [] = (ys, n)
reverseLengthWith ys n (x:xs) =
  let n' = n + 1
  in seq n' (reverseLengthWith (x:ys) n' xs)
