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
  , group
  , groupBy
  , groupAdjacent
  , groupAdjacentBy
  , rotate
  , merge
  , mergeBy
  , mergeMany
  , applyMerge
  ) where

import Control.Monad (guard)
import Data.List     (sort, sortBy, uncons)
import qualified Data.List as List (null)
import Data.Maybe    (fromMaybe)

import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as PQueue

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

{-| @groupAdjacent xs@ groups adjacent elements of xs that are equal. It works
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

-- TODO: Simplify this implementation
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
rotate n xs
  | null xs   = []
  | n >= 0    = let d = case lengthTo n xs of
                          Nothing -> n
                          Just l  -> n `rem` l
                    (ys, zs) = splitAt d xs
                 in zs ++ ys
  | otherwise = let (ys, zs) = splitAt (n `mod` length xs) xs
                 in zs ++ ys

lengthTo :: Int -> [a] -> Maybe Int
lengthTo n xs =
  if (not . null) $ drop n xs
  then Nothing
  else Just (length xs)

{-|
    Merge a list of lists. Works with infinite lists of infinite lists.

    __Preconditions:__ Each list must be sorted, and the list of lists must be sorted by first element.

    >>> take 10 $ mergeMany $ map (\x -> [x..]) [1..]
    [1, 2, 2, 3, 3, 3, 4, 4, 4, 4]
-}
mergeMany :: (Ord a) => [[a]] -> [a]
mergeMany xss = 
  let xss' = filter (not . List.null) xss
   in if List.null xss'
      then []
      else let n = Plane (filter (not . List.null) xss')
            in generate (PQueue.singleton (root n) n)

   where
     generate :: (Ord a) => MinPQueue a (Node a) -> [a]
     generate pq =
       case PQueue.minViewWithKey pq of
         Nothing -> []
         Just ((x, node), pq') -> x:(generate pq'')
           where pq'' = foldr (uncurry PQueue.insert) pq' (children node)


     null :: Node a -> Bool
     null (Plane xss) = List.null xss
     null (Line xs)   = List.null xs

     root :: Node a -> a
     root (Plane xss) = head (head xss)
     root (Line xs) = head xs

     children :: Node a -> [(a, Node a)]
     children node =
        map (\x -> (root x, x))
      $ filter (not . null)
      $ case node of
          (Line (x:xs))        -> [Line xs]
          (Plane ((x:xs):xss)) -> [Line xs, Plane xss]

data Node a = Plane [[a]] | Line [a]
  deriving (Show, Ord, Eq)

{-|
    Given a binary operation `op` and sorted lists xs, ys, @applyMerge op xs ys@
    yields in sorted order, [z | z = x*y, x <- xs, y <- ys]. Works even if xs, ys are
    infinite.

    __Preconditions:__ Each list must be sorted, and the operation must be
    non-decreasing in both arguments. That is,
     * x1 >= x2 => op x1 y >= op x2 y
     * y1 >= y2 => op x y1 >= op x y2
-}
applyMerge :: (Ord a, Ord b, Ord c) => (a -> b -> c) -> [a] -> [b] -> [c]
applyMerge op xs ys = mergeMany $ map (\x -> map (op x) ys) xs

merge :: (Ord a) => [a] -> [a] -> [a]
merge = mergeBy compare

mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy _ [] ys = ys
mergeBy _ xs [] = xs
mergeBy cmp (x:xs) (y:ys)
  | cmp y x == GT = y:(mergeBy cmp (x:xs) ys)
  | otherwise     = x:(mergeBy cmp xs (y:ys))
