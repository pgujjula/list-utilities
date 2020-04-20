{-|
    Module      : Data.List.Ordered.Set
    Description : Work with an ordered list like a set.
    Copyright   : (c) Preetham Gujjula, 2020
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

    Work with an ordered, potentially infinite list like a set.
-}

module Data.List.Ordered.Set
  ( member
  , mkListSet
  , ListSet
  ) where

import Data.List (find)
import Data.Maybe (fromJust)

import Data.Set (Set)
import qualified Data.Set as Set

data Limit a = Infimum | Only a | Supremum
  deriving (Show, Eq, Ord)

data ListSet a =
  ListSet {
    set :: Set a,
    limit :: Limit a,
    horizon :: [a]
  }
  deriving (Show)

mkListSet :: [a] -> ListSet a
mkListSet = ListSet Set.empty Infimum

member :: (Ord a) => a -> ListSet a -> (Bool, ListSet a)
member x cl =
  let cl' = fillTo (Only x) cl
   in (Set.member x (set cl'), cl')

fillTo :: (Ord a) => Limit a -> ListSet a -> ListSet a
fillTo l cl = fromJust
            $ find (\cl' -> limit cl' >= l)
            $ iterate popHorizon cl

popHorizon :: (Ord a) => ListSet a -> ListSet a
popHorizon (ListSet s _ h) =
  case h of
    [] -> ListSet s Supremum []
    (x:xs) -> ListSet (Set.insert x s) (Only x) xs
