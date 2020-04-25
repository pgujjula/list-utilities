{- Utilities for generating lists -}
module Data.List.Gen
    ( Finiteness(Finite, Infinite)
    , Repeatedness(Repeated, Standard)
    , Sortedness(Sorted, Reversed)
    , SortedConfig(SortedConfig, finiteness, repeatedness, sortedness)
    , defaultConfig
    , sortedGen
    , sortedGenWith
    , pairOf
    , forAllInfinite
    ) where

import Test.QuickCheck (Gen, Property, Testable, choose, forAllShow,
                        infiniteListOf, listOf1, oneof)

data Finiteness = Finite | Infinite
    deriving (Show, Enum, Eq, Ord)

data Repeatedness = Repeated -- Elements are repeated a lot
                  | Standard -- Regular amount of repeatedness, some repeats may
                             --   be present.
                  | Unique   -- All elements are unique.
    deriving (Show, Enum, Eq, Ord)

data Sortedness = Sorted | Reversed
    deriving (Show, Enum, Eq, Ord)

data SortedConfig = SortedConfig { finiteness   :: Finiteness
                                 , repeatedness :: Repeatedness
                                 , sortedness   :: Sortedness
                                 }
    deriving (Show, Eq, Ord)

defaultConfig :: SortedConfig
defaultConfig = SortedConfig { finiteness   = Finite
                             , repeatedness = Standard
                             , sortedness   = Sorted
                             }

sortedGen :: Gen [Integer]
sortedGen = sortedGenWith defaultConfig

sortedGenWith :: SortedConfig -> Gen [Integer]
sortedGenWith conf = do
    let diffGen = case repeatedness conf of
                      Standard -> choose (0, 7)
                      Unique   -> choose (1, 7)
                      Repeated -> oneof [return 0, return 0, choose (1, 7)]
    diffs <- case finiteness conf of
                 Finite   -> listOf1 diffGen
                 Infinite -> infiniteListOf diffGen
    let sums = scanl1 (+) diffs
    let orderedSums = case sortedness conf of
                          Sorted   -> sums
                          Reversed -> map negate sums
    return orderedSums

pairOf :: Gen a -> Gen (a, a)
pairOf gen = (,) <$> gen <*> gen

forAllInfinite :: (Show a, Testable prop) => Gen a -> (a -> prop) -> Property
forAllInfinite gen = forAllShow gen (const message)
  where
    message = "<cannot display infinite list(s), inspect through debugging>"
