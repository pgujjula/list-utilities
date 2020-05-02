{-| Module      : Data.List.Filter
    Description : Special takes and drops on lists
    Copyright   : (c) Preetham Gujjula, 2020
    License     : BSD3
    Maintainer  : pgujjula+list-utilities@protonmail.com
    Stability   : experimental

Special takes and drops on lists.
-}
module Data.List.Filter (
      takeEvery
    , dropEvery
    , takeUntil
    , dropUntil
    ) where

{-| @takeEvery n xs@ is a list of every @n@th element of @xs@.

    __Precondition:__ @n@ must be positive.

    >>> takeEvery 3 [1..10]
    [3, 6, 9]
    >>> takeEvery 1 [1..10] == [1..10]
    True
-}
takeEvery :: Int -> [a] -> [a]
takeEvery step xs = compute validated
  where
    compute ys = case drop (step - 1) ys of
                     []    -> []
                     y:ys' -> y : compute ys'
    validated
        | step > 0  = xs
        | otherwise = error $ "Data.List.Transform.takeEvery: Step parameter "
                             ++ "must be positive."

{-| @dropEvery n xs@ is a list of every @n@th element of @xs@.

    __Precondition:__ @n@ must be positive.

    >>> takeEvery 3 [1..10]
    [3, 6, 9]
    >>> takeEvery 1 [1..10] == [1..10]
    True
-}
dropEvery :: Int -> [a] -> [a]
dropEvery step xs = compute validated
  where
    compute ys = case splitAt (step - 1) ys of
                     (as, [])   -> as
                     (as, b:bs) -> as ++ compute bs
    validated
        | step > 0  = xs
        | otherwise = error $ "Data.List.Transform.dropEvery: Step parameter "
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
