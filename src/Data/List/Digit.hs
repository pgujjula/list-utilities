{-|
Module      : Data.List.Digit
Description : Lists of digits to numbers, and vice versa
Copyright   : (c) Preetham Gujjula, 2020
License     : GPL-3
Maintainer  : preetham.gujjula@gmail.com
Stability   : experimental
-}
module Data.List.Digit
  ( numDigits
  , sumDigits
  , fromDigits
  , toDigits
  ) where

import Data.Char (chr, ord)

ordZero :: Int
ordZero = ord '0'

-- TODO: Move this to hilbert
{-|
    @numDigits n@ is the number of digits in @n@.

    >>> numDigits 2938475
    7
    >>> numDigits 0
    1
    >>> numDigits (-38417)
    5
-}
numDigits :: (Integral a) => a -> Int
numDigits = length . show . toInteger . abs

-- TODO: Move this to Hilbert
{-|
    @sumDigits n@ is the sum of the digits in @n@.

    >>> sumDigits 2938475
    38
    >>> sumDigits 0
    0
    >>> sumDigits (-12)
    3
-}
sumDigits :: (Integral a) => a -> Int
sumDigits = sum . toDigits . abs

{-|
    @toDigits n@ is a list of the digits in @n@.

    __Precondition:__ @n@ must be nonnegative, so that @toDigits@ is the
    inverse of @fromDigits@.

    >>> toDigits 2338475
    [2, 3, 3, 8, 4, 7, 5]
    >>> toDigits 0
    [0]
-}
toDigits :: (Integral a) => a -> [Int]
toDigits = map (\x -> ord x - ordZero) . show . toInteger

{-|
    @fromDigits xs@ is converts a list of digits @xs@ to an integer.

    __Precondition:__ All elements of @xs@ are in @[0..9]@.

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
fromDigits = fromInteger . read . map (\x -> chr (x + ordZero)) . (0:)
