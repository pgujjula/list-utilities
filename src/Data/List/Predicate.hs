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

  , ascendingContiguous
  , descendingContiguous

  , palindrome
  ) where

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

-- TODO: Consider whethe to enforce input being sorted
allUnique :: (Ord a) => [a] -> Bool
allUnique = undefined

allUniqueBy :: (a -> a -> Ordering) -> [a] -> Bool
allUniqueBy = undefined

-- TODO Think of a better name
ascendingContiguous :: (Enum a) => [a] -> Bool
ascendingContiguous = undefined

descendingContiguous :: (Enum a) => [a] -> Bool
descendingContiguous = undefined

palindrome :: (Eq a) => [a] -> Bool
palindrome = undefined
