{-| Module      : Data.List.Combinatorial
    Description : Combinatorial transformations of lists.
    Copyright   : (c) Preetham Gujjula, 2020
    License     : BSD3
    Maintainer  : pgujjula+list-utilities@protonmail.com
    Stability   : experimental

Combinatorial transformations of lists.
-}
module Data.List.Combinatorial (
      rotate
    , rotations
    , combinationsOfSize
    , permutationsOfSize
    , derangements
    ) where

import Data.List (inits, tails)

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

{-| All left-rotations of a list. Works with infinite lists.

    >>> rotations [1, 2, 3, 4]
    [[1, 2, 3, 4], [2, 3, 4, 1], [3, 4, 1, 2], [4, 1, 2, 3]]
    >>> take 4 . map (take 4) $ rotations [1..]
    [[1, 2, 3, 4], [2, 3, 4, 5], [3, 4, 5, 6], [4, 5, 6, 7]]
    >>> rotations []
    [[]]
-}
rotations :: [a] -> [[a]]
rotations [] = [[]]
rotations xs = init $ zipWith (++) (tails xs) (inits xs)

{-| All combinations of given size of the list. Elements will retain their
    relative order, and if the original list is ordered, then the combinations
    will also be ordered. Note that if the original list contains duplicates,
    then the combinations will also contain duplicates--elements are not
    evaluated, and are treated as if they are all distinct.

    This function will not hang on infinite lists, but following the ordering
    rules above means some combinations will never be produced, even though the
    combinations are enumerable if we used a different ordering.

    >>> combinationsOfSize 2 [1, 2, 3, 4]
    [[1, 2], [1, 3], [1, 4], [2, 3], [2, 4], [3, 4]]
    >>> combinationsOfSize 0 [1, 2, 3, 4]
    [[]]
    >>> combinationsOfSize 4 [1, 2, 3, 4]
    [[1, 2, 3, 4]]
    >>> take 5 $ combinationsOfSize 3 [1..]
    [[1, 2, 3], [1, 2, 4], [1, 2, 5], [1, 2, 6], [1, 2, 7]]
-}
combinationsOfSize :: Int -> [a] -> [[a]]
combinationsOfSize n xs
  | n < 0   = []
  | n == 0  = [[]]
  | otherwise = case xs of
                  [] -> []
                  (x:xs') -> map (x:) (combinationsOfSize (n - 1) xs')
                          ++ combinationsOfSize n xs'

{-| All permutations of given size of the list. If the original list is sorted,
    permutations are returned in sorted order.

    This function will not hang on infinite lists, but following the ordering
    rules above means some permutations will never be produced, even though the
    permutations are enumerable if we used a different ordering.

    >>> permutationsOfSize 2 [1, 2, 3]
    [[1, 2], [1, 3], [2, 1], [2, 3], [3, 1], [3, 2]]
    >>> permutationsOfSize 0 [1, 2, 3]
    [[]]
-}
permutationsOfSize :: Int -> [a] -> [[a]]
permutationsOfSize n xs
    | n < 0  = []
    | n == 0 = [[]]
    | otherwise = case xs of
                  [] -> []
                  _  -> concat $ zipWith (\x -> map (x:)) xs subPermutations
  where
    deleteEach ys = zipWith (++) (inits ys) (tail $ tails ys)
    subPermutations = map (permutationsOfSize (n - 1)) (deleteEach xs)

{-| Permutations where no element ends up in its original place. If the original
    list is ordered, derangements are returned in sorted order.

    This function will not hang on infinite lists, but following the ordering
    rules above means some derangements will never be produced, even though the
    derangements are enumerable if we used a different ordering.

    >>> derangements [1, 2, 3]
    [[2,3,1],[3,1,2]]
    >>> derangements [1]
    []
    >>> derangements []
    [[]]
-}
derangements :: [a] -> [[a]]
derangements xs = ds
  where
    index = zip [0..]
    getIndices = map fst
    stripIndices = map snd
    derangedIndices = and . zipWith (/=) [(0 :: Int)..]

    xs' = index xs
    ps' = permutationsOfSize (length xs') xs'
    ds' = filter (derangedIndices . getIndices) ps'
    ds = map stripIndices ds'
