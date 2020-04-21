{-|
Module      : Data.List.Digit
Description : Transform lists to other lists.
Copyright   : (c) Preetham Gujjula, 2020
License     : GPL-3
Maintainer  : preetham.gujjula@gmail.com
Stability   : experimental

Transform lists to other lists.
-}
module Data.List.Transform
  ( takeEvery
  ) where

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
