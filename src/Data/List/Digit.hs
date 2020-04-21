module Data.List.Digit (numDigits) where

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
numDigits = numDigitsAbs . abs
  where numDigitsAbs n
          | n < 10    = 1
          | otherwise = 1 + (numDigitsAbs $ n `div` 10)
