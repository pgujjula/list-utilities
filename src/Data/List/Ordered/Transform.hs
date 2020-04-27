{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}
{-| Module      : Data.List.Ordered.Transform
    Description : Transform ordered lists.
    Copyright   : (c) Preetham Gujjula, 2020
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

Transform ordered lists.
-}
module Data.List.Ordered.Transform
  ( -- * Basic mulitset operations
    -- | Basic operations on lists as multisets (sets with repetitions allowed).
    -- All input and output lists in this section are ordered, and the
    -- precondition that input lists are ordered is __unchecked__.
    --
    -- The "by" versions of the functions work on the equivalence classes
    -- induced by the comparision function. The representatives of each class
    -- may be distinguisable to the user, so the user may be be concerned with
    -- how ties between representatives are resolved. The tie-breaking behavior
    -- for each function is described in the "stability" section.
    --
    -- All functions run in linear time, and work with infinite lists.
    merge
  , mergeBy
  , diff
  , diffBy
  , intersect
  , intersectBy
  , union
  , unionBy

  -- * Advanced merging
  , mergeMany
  , applyMerge
  ) where

import           Data.List            (foldl')
import           Data.List.NonEmpty   (NonEmpty ((:|)), nonEmpty)
import           Data.Maybe           (catMaybes, mapMaybe)

import           Data.PQueue.Prio.Min (MinPQueue, minViewWithKey)
import qualified Data.PQueue.Prio.Min as PQueue

{-| Merge two lists.

    >>> merge [1, 2, 3] [1, 2, 4]
    [1, 1, 2, 2, 3, 4]
-}
merge :: (Ord a) => [a] -> [a] -> [a]
merge = mergeBy compare

{-| Like 'merge', with a custom comparison function.

    __Stability:__ The left side is preferred on ties.

    >>> let xs = ["a1", "a2",       "b1",     ]
    >>> let ys = ["a3", "a4", "a5",       "c1"]
    >>> mergeBy (comparing head) xs ys
    ["a1", "a2", "a3", "a4", "a5", "b1", "c1"]
-}
mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy _ [] ys = ys
mergeBy _ xs [] = xs
mergeBy cmp (x:xs) (y:ys)
    | cmp y x /= LT = x : mergeBy cmp xs (y:ys)
    | otherwise     = y : mergeBy cmp (x:xs) ys

{-| Yield the elements in the first list that are not in the second, with
    multiplicities considered.

    >>> diff [1, 1, 1, 1, 2] [1, 1, 3]
    [1, 1, 2]
    >>> diff [] undefined
    []
-}
diff :: (Ord a) => [a] -> [a] -> [a]
diff = diffBy compare

{-| Like 'diff' with a custom comparison function.

    __Stability:__ If the left side has @x@ representatives of an equivalence
    class @n@, and the right side has @y@, then the last @max(x - y, 0)@
    representatives from the left side used for @n@ in the output. 

    >>> let xs = ["a1", "a2",       "b1",     ]
    >>> let ys = ["a3", "a4", "a5",       "c1"]
    >>> diffBy (comparing head) xs ys
    ["b1"]
    >>> diffBy (comparing head) ys xs
    ["a5", "c1"]

    Observe that @"a5"@ is the /last/ representative of the "a" class in @ys@.
-}
diffBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
diffBy _ [] _  = []
diffBy _ xs [] = xs
diffBy cmp (x:xs) (y:ys) =
    case cmp x y of
        LT -> x : diffBy cmp xs (y:ys)
        EQ -> diffBy cmp xs ys
        GT -> diffBy cmp (x:xs) ys

{-| Yield the elements in both lists, with multiplicities considered.

    >>> intersect [1, 2, 3, 3, 3, 4, 5, 6] [2, 3, 3, 5, 7]
    [2, 3, 3, 5]
-}
intersect :: (Ord a) => [a] -> [a] -> [a]
intersect = intersectBy compare

{-| Like 'intersect' with a custom comparison function. 

    __Stability:__ If the left side has @x@ representatives of an equivalence
    class @n@, and the right side has @y@, then the first @min(x, y)@
    representatives from the left side are used for @n@ in the output.

    >>> let xs = ["a1", "a2",       "b1",     ]
    >>> let ys = ["a3", "a4", "a5",       "c1"]
    >>> intersectBy (comparing head) xs ys
    ["a1", "a2"]
-}
intersectBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
intersectBy _ [] _ = []
intersectBy _ _ [] = []
intersectBy cmp (x:xs) (y:ys) =
    case cmp x y of
        LT -> intersectBy cmp xs (y:ys)
        EQ -> x : intersectBy cmp xs ys
        GT -> intersectBy cmp (x:xs) ys

{-| Yield the elements in either list, with multiplicities considered.

    >>> union [1, 3, 3, 4, 5] [2, 3, 5, 7]
    [1, 2, 3, 3, 4, 5, 7]
-}
union :: (Ord a) => [a] -> [a] -> [a]
union = unionBy compare

{-| Like 'union', with a custom comparison function.

    __Stability:__ If left side has @x@ representatives of an equivalence class
    @n@, and the right side has @y@, then:

     * If @x >= y@, the @x@ representatives from the left will be used for @n@
       in the output.
     * If @x < y@, then the first @x@ representatives from the left and __last__
       @y - x@ representatives from the right will be used.

    >>> let xs = ["a1", "a2",       "b1",     ]
    >>> let ys = ["a3", "a4", "a5",       "c1"]
    >>> unionBy (comparing head) xs ys
    ["a1", "a2", "a5", "b1", "c1"]

    Observe that @"a5"@ is the /last/ representative of the "a" class in @ys@.
 -}
unionBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
unionBy _ [] xs = xs
unionBy _ xs [] = xs
unionBy cmp (x:xs) (y:ys) =
    case cmp x y of
        LT -> x : unionBy cmp xs (y:ys)
        EQ -> x : unionBy cmp xs ys
        GT -> y : unionBy cmp (x:xs) ys

-- mergeMany algorithm
-- ===================
-- This function uses a priority queue to generate elements in the right order.
-- Consider a list of lists:
--   [[1, 2, 3..], [2, 4, 6..], [3, 6, 9..], ..]
-- It can be represented in the following tree structure:
--   1 - 2 - 3 - ...
--    \
--     2 - 4 - 6 - ...
--      \
--       3 - 6 - 9 - ...
--        \
--         ...
-- We denote a value along with its children as a Segment. There are two kinds
-- of Segments: Trees and Branches,
-- and every Segment has a root node, and maybe some Segments as children. For
-- example, in the structure above,
-- the 1 is rooting a Tree, while the 2 on its right is rooting a Branch. The 2
-- below is rooting another Tree.
--
-- It's clear a Tree has at most one Tree and one Branch as children, while a
-- Branch has at most another Branch
-- as a child.
--
-- To generate all the values in the structure in sorted order, we maintain a
-- priority queue of all unprocessed
-- Segments, prioritied by their root node. The queue is initialized with a
-- single Tree, representing the entire
-- structure. To generate an element, we pop the minimum Segment, yield the root
-- node, and reinsert the children.
-- If the queue becomes empty, we have yielded the entire list, so we stop.

data Segment a = Branch (NonEmpty a)
               | Tree (NonEmpty (NonEmpty a))
  deriving (Show)

getRoot :: Segment a -> a
getRoot (Branch (x :| _))      = x
getRoot (Tree ((x :| _) :| _)) = x

children :: Segment a -> [Segment a]
children (Branch (_ :| xs)) = catMaybes [Branch <$> nonEmpty xs]
children (Tree ((_ :| xs) :| xss)) =
    catMaybes [Branch <$> nonEmpty xs, Tree <$> nonEmpty xss]

{-| Merge a list of lists. Works with infinite lists of infinite lists.

    __Preconditions:__ Each list must be ordered, and the list of lists must be
    ordered by first element.

    __Stability:__ No guarantees are made regarding stability. Equal elements may
    be returned in any relative order.

    >>> take 10 $ mergeMany $ map (\x -> [x..]) [1..]
    [1, 2, 2, 3, 3, 3, 4, 4, 4, 4]
-}
mergeMany :: (Ord a) => [[a]] -> [a]
mergeMany xss =
    case nonEmpty $ mapMaybe nonEmpty xss of
        Nothing   -> []
        Just nxss -> generate $ PQueue.singleton (getRoot tree) tree
          where
            tree = Tree nxss

  where
    generate :: (Ord a) => MinPQueue a (Segment a) -> [a]
    generate queue =
        case minViewWithKey queue of
            Nothing -> []
            Just ((root, segment), queue') -> root : generate queue''
              where
                queue'' = foldl' insertSegment queue' (children segment)

    insertSegment :: (Ord a)
                  => MinPQueue a (Segment a)
                  -> Segment a
                  -> MinPQueue a (Segment a)
    insertSegment queue segment = PQueue.insert (getRoot segment) segment queue

-- TODO: Improve this explanation.
{-| Given a binary operation `op` and sorted lists xs, ys, @applyMerge op xs ys@
    yields in sorted order, [z | z = x*y, x <- xs, y <- ys]. Works even if xs,
    ys are infinite.

    __Preconditions:__ Each list must be sorted, and the operation must be
    non-decreasing in both arguments. That is,
     * x1 >= x2 => op x1 y >= op x2 y
     * y1 >= y2 => op x y1 >= op x y2

    __Stability:__ No guarantees are made regarding stability. Equal elements may be returned in any relative order.
-}
applyMerge :: (Ord c) => (a -> b -> c) -> [a] -> [b] -> [c]
applyMerge op xs ys = mergeMany $ map (\x -> map (op x) ys) xs
