{-# LANGUAGE TupleSections #-}
module Data.List.Ordered.TransformSpec (spec) where

import Control.Arrow               ((>>>))
import Data.List                   (foldl1', sort, transpose, (\\))
import Data.Ord                    (Down (Down), comparing)

import Test.Hspec                  (Expectation, Spec, describe, it, shouldBe,
                                    shouldStartWith)
import Test.QuickCheck             (Gen, Property, forAll, (===))

import Data.List.Gen               (Finiteness (Infinite),
                                    Repeatedness (Repeated), defaultConfig,
                                    finiteness, forAllInfinite, pairOf,
                                    repeatedness, sortedGen, sortedGenWith)
import Data.List.Ordered.Transform (applyMerge, diff, diffBy, intersect,
                                    intersectBy, merge, mergeBy, mergeMany,
                                    union, unionBy)

-- TODO: figure out a way to write a custom forAll function that automatically
-- compares truncated infinite lists, or decide that it's not possible
infiniteListTruncationLength :: Int
infiniteListTruncationLength = 100

trunc :: [a] -> [a]
trunc = take infiniteListTruncationLength

spec :: Spec
spec = do
    describe "merge" mergeSpec
    describe "mergeBy" mergeBySpec

    describe "diff" diffSpec
    describe "diffBy" diffBySpec

    describe "intersect" intersectSpec
    describe "intersectBy" intersectBySpec

    describe "union" unionSpec
    describe "unionBy" unionBySpec

    describe "test functions together" togetherSpec

    describe "mergeMany" mergeManySpec
    describe "applyMerge" applyMergeSpec

mergeSpec :: Spec
mergeSpec = do
    it "both empty" $
        merge [] [] `shouldBe` ([] :: [()])
    it "left empty" $
        merge [] [1, 2, 3] `shouldBe` [1, 2, 3]
    it "right empty" $
        merge [1, 2, 3] [] `shouldBe` [1, 2, 3]
    it "finite lists" $
        merge [1, 2, 3] [1, 3, 5] `shouldBe` [1, 1, 2, 3, 3, 5]
    it "infinite lists" $
        trunc (merge [2, 4..] [1, 3..]) `shouldBe` trunc [1..]

    it "arbitrary finite lists with many repeats" $
        let gen = sortedGenWith defaultConfig {repeatedness = Repeated}
         in forAll (pairOf gen) $ \(xs, ys) ->
                merge xs ys === sort (xs ++ ys)
    it "arbitrary infinite lists with many repeats" $
        let gen = sortedGenWith defaultConfig { repeatedness = Repeated
                                              , finiteness = Infinite}
         in forAllInfinite (pairOf gen) $ \(xs, ys) ->
                trunc (merge xs ys) === trunc (sort $ trunc xs ++ trunc ys)

mergeBySpec :: Spec
mergeBySpec = do
    it "both empty" $
        mergeBy undefined [] [] `shouldBe` ([] :: [()])
    it "left empty" $
        mergeBy undefined [] [1, 2, 3] `shouldBe` [1, 2, 3]
    it "right empty" $
        mergeBy undefined [1, 2, 3] [] `shouldBe` [1, 2, 3]
    it "finite lists" $ do
        let cmp = comparing Down
        mergeBy cmp [3, 2, 1] [5, 3, 1] `shouldBe` [5, 3, 3, 2, 1, 1]
    it "left side preferred" $
        let xs = map ("x",) [1, 3, 4, 4, 5]
            ys = map ("y",) [1, 3, 3, 4, 5]
            zs = [("x", 1), ("y", 1), ("x", 3), ("y", 3), ("y", 3),
                  ("x", 4), ("x", 4), ("y", 4), ("x", 5), ("y", 5)]
         in mergeBy (comparing snd) xs ys `shouldBe` zs

diffSpec :: Spec
diffSpec = do
    it "both empty" $
        diff [] [] `shouldBe` ([] :: [()])
    it "left empty" $
        diff [] undefined `shouldBe` ([] :: [()])
    it "right empty" $
        diff [1, 2, 3] [] `shouldBe` [1, 2, 3]
    it "finite lists" $
        diff [3, 4, 4, 4, 4, 5, 5] [3, 4, 4, 5, 6] `shouldBe` [4, 4, 5]
    it "infinite lists" $
        let xs = [1..]
            ys = map (^2) [1..]
            square x = any (\t -> t^2 == x) [1..x]
         in filter (not . square) xs `shouldStartWith` trunc (diff xs ys)

    it "arbitrary finite lists" $
        forAll (pairOf sortedGen) $ \(xs, ys) ->
            diff xs ys `shouldBe` (xs \\ ys)
    -- It's too hard to construct an arbitrary infinite lists test. The simplest
    -- way to determine a prefix of the difference of two infinite lists is
    -- probably to construct the diff algorithm itself. So we settle for a hand-
    -- written unit test for that case.

diffBySpec :: Spec
diffBySpec = do
    it "both empty" $
        diffBy undefined [] [] `shouldBe` ([] :: [()])
    it "left empty" $
        diffBy undefined [] undefined `shouldBe` ([] :: [()])
    it "right empty" $
        diffBy undefined [1, 2, 3] [] `shouldBe` [1, 2, 3]
    it "finite lists" $
        diffBy (comparing Down) [4, 3, 3, 2, 1] [3, 2, 1] `shouldBe` [4, 3]

intersectSpec :: Spec
intersectSpec = do
    it "both empty" $
        intersect [] [] `shouldBe` ([] :: [()])
    it "left empty" $
        intersect [] [1, 2, 3] `shouldBe` []
    it "right empty" $
        intersect [1, 2, 3] [] `shouldBe` []
    it "finite lists" $
        intersect [1, 2, 3, 3, 3, 4, 5, 6] [2, 3, 3, 5, 7] `shouldBe`
            [2, 3, 3, 5]
    it "infinite lists" $
        let xs = map (^2) [1..]
            ys = map (^3) [1..]
            zs = map (^6) [1..]
         in trunc (xs `intersect` ys) `shouldBe` trunc zs

    -- Since the Data.List implementation of intersect doesn't have the multiset
    -- semantics of this impelementation, we can't use it as a reference for
    -- auto-generated QuickCheck tests. Implementing a "naive" intersect is also
    -- difficult. So we settle for hand-written unit tests.

intersectBySpec :: Spec
intersectBySpec = do
    it "both empty" $
        intersectBy undefined [] [] `shouldBe` ([] :: [()])
    it "left empty" $
        intersectBy undefined [] [1, 2, 3] `shouldBe` []
    it "right empty" $
        intersectBy undefined [1, 2, 3] [] `shouldBe` []
    it "finite lists" $
        intersectBy (comparing Down) [3, 2, 1] [3, 1, 0] `shouldBe` [3, 1]
    it "left side preferred" $
        let xs = map ("x",) [1, 2, 3, 4]
            ys = map ("y",) [1, 2, 3, 5]
            zs = map ("x",) [1, 2, 3]
         in intersectBy (comparing snd) xs ys `shouldBe` zs

unionSpec :: Spec
unionSpec = do
    it "both empty" $
        union [] [] `shouldBe` ([] :: [()])
    it "left empty" $
        union [] [1, 2, 3] `shouldBe` [1, 2, 3]
    it "right empty" $
        union [1, 2, 3] [] `shouldBe` [1, 2, 3]
    it "finite lists" $
        union [1, 3, 3, 4, 5] [2, 3, 5, 7] `shouldBe` [1, 2, 3, 3, 4, 5, 7]
    it "infinite lists" $
        let xs = filter (\t -> t `rem` 4 == 1) [1..]
            ys = filter (\t -> t `rem` 4 == 3) [1..]
            zs = filter odd [1..]
         in trunc (xs `union` ys) `shouldBe` trunc zs

    -- The same problem with intersect (described above) means that its hard to
    -- make QuickCheck tests, so we leave it at hand-written unit tests.

unionBySpec :: Spec
unionBySpec = do
    it "both empty" $
        unionBy undefined [] [] `shouldBe` ([] :: [()])
    it "left empty" $
        unionBy undefined [] [1, 2, 3] `shouldBe` [1, 2, 3]
    it "right empty" $
        unionBy undefined [1, 2, 3] [] `shouldBe` [1, 2, 3]
    it "finite lists" $
        unionBy (comparing Down) [3, 2, 1] [3, 1, 0] `shouldBe` [3, 2, 1, 0]
    it "left side preferred" $
        let xs = map ("x",) [1, 2, 3, 4]
            ys = map ("y",) [1, 2, 3, 5]
            zs = [("x", 1), ("x", 2), ("x", 3), ("x", 4), ("y", 5)]
         in unionBy (comparing snd) xs ys `shouldBe` zs

togetherSpec :: Spec
togetherSpec = it "x ∪ y == (x ∩ y) + (x - y) + (y - x)" $
    let gen = sortedGenWith defaultConfig {repeatedness = Repeated}
     in forAll (pairOf gen) $ \(xs, ys) ->
            union xs ys === foldl1' merge [ xs `intersect` ys
                                          , xs `diff` ys
                                          , ys `diff` xs
                                          ]

mergeManySpec :: Spec
mergeManySpec = do
    let naive :: (Ord a) => [[a]] -> [a]
        naive = filter (not . null)
            >>> trunc
            >>> map trunc
            >>> concat
            >>> sort
            >>> trunc

        hunitTest :: (Ord a, Show a) => [[a]] -> Expectation
        hunitTest xss = trunc (mergeMany xss) `shouldBe` naive xss

        -- (products xs ys) !! i !! j == (xs !! i) * (ys !! j)
        products :: (Num a) => [a] -> [a] -> [[a]]
        products xs ys = map (\x -> map (*x) ys) xs

        productsGen :: (Num a, Show a) => Gen [a] -> Gen [a] -> Gen [[a]]
        productsGen g1 g2 = products <$> g1 <*> g2

    it "empty list" $
        hunitTest ([] :: [[()]])
    it "list of empty lists" $
        hunitTest (replicate 10 ([] :: [()]))
    it "finite list of finite lists" $
        hunitTest $ products [1, 2, 3] [1, 2, 3]
    it "finite list of infinite lists" $
        hunitTest $ products [1, 2, 3] [1..]
    it "infinite list of finite lists" $
        hunitTest $ products [1..] [1, 2, 3]
    it "infinite list of infinite lists" $
        hunitTest $ products [1..] [1..]
    it "ragged finite list" $
        hunitTest [[], [], [1, 2, 3], [1, 2], [2, 5], [], [], [3, 3, 3, 4]]

    let qcTest :: (Ord a, Show a) => [[a]] -> Property
        qcTest xss = trunc (mergeMany xss) === naive xss

        infiniteSortedGen :: Gen [Integer]
        infiniteSortedGen = sortedGenWith defaultConfig {finiteness = Infinite}

        testProduct :: (Num a, Show a, Ord a) => Gen [a] -> Gen [a] -> Property
        testProduct gen1 gen2 = forAllInfinite (productsGen gen1 gen2) qcTest

    it "arbitrary finite lists of finite lists" $
        testProduct sortedGen sortedGen
    it "arbitrary infinite lists of finite lists" $
        testProduct infiniteSortedGen sortedGen
    it "arbitrary finite lists of infinite lists" $
        testProduct sortedGen infiniteSortedGen
    it "arbitrary infinite lists of infinite lists" $
        testProduct infiniteSortedGen infiniteSortedGen

    it "transposition test" $
        forAllInfinite (productsGen infiniteSortedGen infiniteSortedGen) $
            \xss -> trunc (mergeMany xss) == trunc (mergeMany $ transpose xss)

applyMergeSpec :: Spec
applyMergeSpec = do
    it "both empty" $
        applyMerge undefined [] [] `shouldBe` ([] :: [()])
    it "left empty" $
        applyMerge undefined [] [1, 2, 3] `shouldBe` ([] :: [Int])
    it "right empty" $
        applyMerge undefined [1, 2, 3] [] `shouldBe` ([] :: [Int])

    let naive :: (Ord c) => (a -> b -> c) -> [a] -> [b] -> [c]
        naive op xs ys = trunc $ sort (op <$> trunc xs <*> trunc ys)

        hunitTest :: (Show c, Ord c)
                  => (a -> b -> c) -> [a] -> [b] -> Expectation
        hunitTest op xs ys =
            trunc (applyMerge op xs ys) `shouldBe` naive op xs ys

    it "both finite" $
        hunitTest (*) [2, 4, 8] [3, 6, 9]
    it "left finite, right infinite" $
        hunitTest (*) [1..] [1, 2, 3]
    it "left infinite, right finite" $
        hunitTest (*) [1, 2, 3] [1..3]
    it "left finite, right infinite" $
        hunitTest (*) [1..] [1..]
    it "unequal types" $ hunitTest take [1..] (map repeat ['a'..'z'])

    let infiniteSortedGen :: Gen [Integer]
        infiniteSortedGen = sortedGenWith defaultConfig {finiteness = Infinite}

        qcTest :: (Ord c, Show c) => (a -> b -> c) -> ([a], [b]) -> Property
        qcTest op (xs, ys) = trunc (applyMerge op xs ys) === naive op xs ys

        testProduct :: (Ord a, Show a, Num a) => Gen [a] -> Gen [a] -> Property
        testProduct g1 g2 = forAllInfinite ((,) <$> g1 <*> g2) (qcTest (*))

    it "arbitrary finite, finite" $
        testProduct sortedGen sortedGen
    it "arbitrary finite, infinite" $
        testProduct sortedGen infiniteSortedGen
    it "arbitrary finite, infinite" $
        testProduct infiniteSortedGen sortedGen
    it "arbitrary infinite, infinite" $
        testProduct infiniteSortedGen infiniteSortedGen
