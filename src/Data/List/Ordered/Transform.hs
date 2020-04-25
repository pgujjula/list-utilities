{-# LANGUAGE FlexibleContexts, UndecidableInstances, ScopedTypeVariables #-}
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

  , union
  , unionBy

  , mergeMany
  , mergeManyBy

  , applyMerge
  , applyMergeBy
  ) where

import Data.Reflection
import Data.Proxy (Proxy(Proxy))
import Data.PQueue.Prio.Min (MinPQueue, minViewWithKey)
import qualified Data.PQueue.Prio.Min as PQueue

import           Data.List.NonEmpty ( NonEmpty( (:|) ) , nonEmpty)
import Data.Maybe (catMaybes)

{-| Merge two ordered lists. Works lazily on infinite lists. Left side is preferred on ties.
    
    >>> merge [2, 4, 6, 8] [1, 3, 5, 7]
    [1, 2, 3, 4, 5, 6, 7]
-}
merge :: (Ord a) => [a] -> [a] -> [a]
merge = mergeBy compare

{-| Merge two lists with a custom comparison function. The two lists are
    assumed to be ordered by the same function. Left side is preferred on ties.
    
    >>> mergeBy (comparing Down) [8, 6, 4, 2] [7, 5, 3, 1]
    [8, 7, 6, 5, 4, 3, 2, 1]
-}
mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy _ [] ys = ys
mergeBy _ xs [] = xs
mergeBy cmp (x:xs) (y:ys)
  | cmp y x /= LT = x:(mergeBy cmp xs (y:ys))
  | otherwise     = y:(mergeBy cmp (x:xs) ys)

{-| Yields all the elements in the first list that are not in the second, with
    multiplicities considered. In other words, this is a diff of multisets.

    >>> diff [1, 2, 3, 3, 3, 4, 5, 6] [2, 3, 5, 7]
    [1, 3, 3, 4, 6]
-}
diff :: (Ord a) => [a] -> [a] -> [a]
diff = diffBy compare

{-| Like @diff@ with a custom comparison function. -}
diffBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
diffBy _ [] _ = []
diffBy _ xs [] = xs
diffBy cmp (x:xs) (y:ys) =
  case cmp x y of
    LT -> x:(diffBy cmp xs (y:ys))
    EQ -> diffBy cmp xs ys
    GT -> diffBy cmp (x:xs) ys

{-| Yields all the elements in either list, with
    multiplicities considered. The number of times x is in the output is the
    max of how many times it is in each list. 

    >>> union [1, 3, 3, 4, 5] [2, 3, 5, 7]
    [1, 2, 3, 3, 4, 5, 7]
-}
union :: (Ord a) => [a] -> [a] -> [a]
union = unionBy compare

{-| Like @union@ with a custom comparison function. Left side is preferred in case
    of matching elements.
 -}
unionBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
unionBy _ [] xs = xs
unionBy _ xs [] = xs
unionBy cmp (x:xs) (y:ys) =
  case cmp x y of
    LT -> x:(unionBy cmp xs (y:ys))
    EQ -> x:(unionBy cmp xs ys)
    GT -> y:(unionBy cmp (x:xs) ys)

{-| Yields all the elements in both lists with multiplicities considered.

    >>> intersect [1, 2, 3, 3, 3, 4, 5, 6] [2, 3, 3, 5, 7]
    [2, 3, 3, 5]
-}
intersect :: (Ord a) => [a] -> [a] -> [a]
intersect = intersectBy compare

{-| Like @intersect@ with a custom compaison function. Left side is preferred as
    the "representative" of a matching pair in case of ties.
-}
intersectBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
intersectBy _ [] _ = []
intersectBy _ _ [] = []
intersectBy cmp (x:xs) (y:ys) =
  case cmp x y of
    LT -> intersectBy cmp xs (y:ys)
    EQ -> x:(intersectBy cmp xs ys)
    GT -> intersectBy cmp (x:xs) ys

-- mergeMany algorithm
-- ===================
-- This function uses a priority queue to generate elements in the right order. Consider a list of lists:
--   [[1, 2, 3..], [2, 4, 6..], [3, 6, 9..], ..]
-- It can be represented in the following tree structure:
--   1 - 2 - 3 - ...
--    \
--     2 - 4 - 6 - ...
--      \
--       3 - 6 - 9 - ... 
--        \
--         ...
-- We denote a value along with its children as a Segment. There are two kinds of Segments: Trees and Branches,
-- and every Segment has a root node, and maybe some Segments as children. For example, in the structure above,
-- the 1 is rooting a Tree, while the 2 on its right is rooting a Branch. The 2 below is rooting another Tree.
--
-- It's clear a Tree has at most one Tree and one Branch as children, while a Branch has at most another Branch
-- as a child.
--
-- To generate all the values in the structure in sorted order, we maintain a priority queue of all unprocessed
-- Segments, prioritied by their root node. The queue is initialized with a single Tree, representing the entire
-- structure. To generate an element, we pop the minimum Segment, yield the root node, and reinsert the children. 
-- If the queue becomes empty, we have yielded the entire list, so we stop.

data Segment a = Branch (NonEmpty a)
               | Tree (NonEmpty (NonEmpty a))
  deriving (Show)

getRoot :: Segment a -> a
getRoot (Branch (x :| _)) = x
getRoot (Tree ((x :| _) :| _)) = x

children :: Segment a -> [Segment a]
children (Branch (_ :| xs))        =
  catMaybes [Branch <$> nonEmpty xs]
children (Tree ((_ :| xs) :| xss)) =
  catMaybes [Branch <$> nonEmpty xs, Tree <$> nonEmpty xss]

generate :: (Ord a) => MinPQueue a (Segment a) -> [a]
generate queue =
  case minViewWithKey queue of
    Nothing -> []
    Just ((root, segment), queue') -> do
      let queue'' = foldr (\child -> PQueue.insert (getRoot child) child)
                          queue'
                          (children segment)
       in root:(generate queue'')

{-| Merge a list of lists. Works with infinite lists of infinite lists.

    __Preconditions:__ Each list must be sorted, and the list of lists must be sorted by first element.

    >>> take 10 $ mergeMany $ map (\x -> [x..]) [1..]
    [1, 2, 2, 3, 3, 3, 4, 4, 4, 4]
-}
mergeMany :: (Ord a) => [[a]] -> [a]
mergeMany xss = 
  case nonEmpty $ catMaybes $ map nonEmpty xss of
    Nothing -> []
    Just nxss -> let tree = Tree nxss
                  in generate $ PQueue.singleton (getRoot tree) tree

data ReflectedOrd s a = ReflectOrd a

reflectOrd :: Proxy s -> a -> ReflectedOrd s a
reflectOrd _ a = ReflectOrd a

unreflectOrd :: ReflectedOrd s a -> a
unreflectOrd (ReflectOrd a) = a

data ReifiedOrd a = ReifiedOrd {
  reifiedEq :: a -> a -> Bool,
  reifiedCompare :: a -> a -> Ordering }

-- | Creates a `ReifiedOrd` with a comparison function. The equality function
--   is deduced from the comparison.
fromCompare :: (a -> a -> Ordering) -> ReifiedOrd a
fromCompare ord = ReifiedOrd {
  reifiedEq = \x y -> ord x y == EQ,
  reifiedCompare = ord }

instance Reifies s (ReifiedOrd a) => Eq (ReflectedOrd s a) where
  (==) (ReflectOrd x) (ReflectOrd y) =
    reifiedEq (reflect (Proxy :: Proxy s)) x y

instance Reifies s (ReifiedOrd a) => Ord (ReflectedOrd s a) where
  compare (ReflectOrd x) (ReflectOrd y) =
    reifiedCompare (reflect (Proxy :: Proxy s)) x y

mergeManyBy :: (a -> a -> Ordering) -> [[a]] -> [a]
mergeManyBy ord l =
  reify (fromCompare ord) $ \p ->
    map unreflectOrd . mergeMany . map (map (reflectOrd p)) $ l

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
applyMerge = applyMergeBy compare

applyMergeBy :: (c -> c -> Ordering) -> (a -> b -> c) -> [a] -> [b] -> [c]
applyMergeBy cmp op xs ys = mergeManyBy cmp $ map (\x -> map (op x) ys) xs
