{-| Module      : Data.List.Digit
    Description : Lists of digits to numbers, and vice versa
    Copyright   : (c) Preetham Gujjula, 2020
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

Lists of digits to numbers, and vice versa. Function behavior is undefined for
inputs that don't satisfy the preconditions.
-}
module Data.List.Digit
    ( fromDigits
    , toDigits
    ) where

import Data.Char (chr, ord)

ordZero :: Int
ordZero = ord '0'

{-| Convert a list of digits to an integer.

    __Precondition:__ All elements of the input are in @[0..9]@.

    >>> fromDigits [1, 7, 2, 9]
    1729
    >>> fromDigits []
    0
    >>> fromDigits [0, 0, 0]
    0
    >>> fromDigits [0, 4, 2]
    42
-}
fromDigits :: (Integral a) => [Int] -> a
fromDigits = fromInteger . read . map (\x -> chr (x + ordZero)) . (0 :)

{-| Generate the list of digits in the input.

    __Precondition:__ The input must be nonnegative. The domain is restricted so
    that the invariant @(fromDigits . toDigits) n == n@ holds for any @n@ in the
    domain of @toDigits@. This wouldn't be possible if we allowed things like
    @toDigits (-12) == [1, 2]@.

    >>> toDigits 2338475
    [2, 3, 3, 8, 4, 7, 5]
    >>> toDigits 0
    [0]
-}
toDigits :: (Integral a) => a -> [Int]
toDigits = map (\x -> ord x - ordZero) . show . toInteger
