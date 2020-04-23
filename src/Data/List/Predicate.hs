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
  ) where

allEqual :: (Eq a) => [a] -> Bool
allEqual = undefined

allEqualBy :: (a -> a -> Bool) -> [a] -> Bool
allEqualBy = undefined

sorted :: (Ord a) => [a] -> Bool
sorted = undefined

sortedBy :: (a -> a -> Ordering) -> [a] -> Bool
sortedBy = undefined

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
