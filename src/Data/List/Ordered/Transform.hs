{-| Module      : Data.List.Ordered.Transform
    Description : Transform ordered lists to other ordered lists.
    Copyright   : (c) Preetham Gujjula, 2020
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

    Transform ordered lists to other ordered lists.
-}
module Data.List.Ordered.Transform
  ( merge
  , mergeBy
  , diff
  , diffBy
  , intersect
  , intersectBy
  , mergeMany
  , mergeManyBy
  , applyMerge
  , applyMergeBy
  ) where

import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as PQueue

import qualified Data.List as List (null)

{-| Merge two ordered lists. Works lazily on infinite lists.
    
    >>> merge [2, 4, 6, 8] [1, 3, 5, 7]
    [1, 2, 3, 4, 5, 6, 7]
-}
merge :: (Ord a) => [a] -> [a] -> [a]
merge = mergeBy compare

{-| Merge two lists with a custom comparison function. The two lists are
    assumed to be ordered by the same function.
    
    >>> mergeBy (comparing Down) [8, 6, 4, 2] [7, 5, 3, 1]
    [8, 7, 6, 5, 4, 3, 2, 1]
-}
mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy _ [] ys = ys
mergeBy _ xs [] = xs
mergeBy cmp (x:xs) (y:ys)
  | cmp y x == GT = y:(mergeBy cmp (x:xs) ys)
  | otherwise     = x:(mergeBy cmp xs (y:ys))

diff :: (Ord a) => [a] -> [a] -> [a]
diff = undefined

diffBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
diffBy = undefined

intersect :: (Ord a) => [a] -> [a] -> [a]
intersect = undefined

intersectBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
intersectBy = undefined

-- TODO: Reconsider this implementation
{-| Merge a list of lists. Works with infinite lists of infinite lists.

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

mergeManyBy :: (a -> a -> Ordering) -> [[a]] -> [a]
mergeManyBy = undefined

-- TODO: Improve this explanation.
{-| Given a binary operation `op` and sorted lists xs, ys, @applyMerge op xs ys@
    yields in sorted order, [z | z = x*y, x <- xs, y <- ys]. Works even if xs, ys are
    infinite.

    __Preconditions:__ Each list must be sorted, and the operation must be
    non-decreasing in both arguments. That is,
     * x1 >= x2 => op x1 y >= op x2 y
     * y1 >= y2 => op x y1 >= op x y2
-}
applyMerge :: (Ord c) => (a -> b -> c) -> [a] -> [b] -> [c]
applyMerge op xs ys = mergeMany $ map (\x -> map (op x) ys) xs

applyMergeBy :: (c -> c -> Ordering) -> (a -> b -> c) -> [a] -> [b] -> [c]
applyMergeBy = undefined
