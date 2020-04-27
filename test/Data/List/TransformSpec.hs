module Data.List.TransformSpec (spec) where

import Data.Function       (on)
import Data.List           (nub, sort)
import Data.Ord            (Down (Down), comparing)

import Test.Hspec          (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.QuickCheck     (Gen, Property, choose, forAll, listOf, (===))

import Data.List.Gen       (Finiteness (Infinite), Repeatedness (Repeated),
                            defaultConfig, finiteness, repeatedness,
                            sortedGenWith)
import Data.List.Predicate (allAdjUnique, allEqual, allUnique)
import Data.List.Transform (deleteAdjDups, deleteAdjDupsBy, deleteDups,
                            deleteDupsBy, dropUntil, group, groupAdj,
                            groupAdjBy, groupBy, rotate, takeEvery, takeUntil)

infiniteListTruncationLength :: Int
infiniteListTruncationLength = 100

trunc :: [a] -> [a]
trunc = take infiniteListTruncationLength

spec :: Spec
spec = do
    describe "takeEvery" takeEverySpec
    describe "takeUntil" takeUntilSpec
    describe "dropUntil" dropUntilSpec

    describe "group" groupSpec
    describe "groupBy" groupBySpec
    describe "groupAdj" groupAdjSpec
    describe "groupAdjBy" groupAdjBySpec

    describe "deleteDups" deleteDupsSpec
    describe "deleteDupsBy" deleteDupsBySpec
    describe "deleteAdjDups" deleteAdjDupsSpec
    describe "deleteAdjDupsBy" deleteAdjDupsBySpec

    describe "rotate" rotateSpec

empty :: [Integer]
empty = []

singletonUndef :: [Integer]
singletonUndef = [undefined]

takeEverySpec :: Spec
takeEverySpec = do
    it "n = 1, empty list" $
        takeEvery 1 empty `shouldBe` empty
    it "n = 1, finite list" $
        takeEvery 1 [1..10] `shouldBe` [1..10]
    it "n = 1, prefix of infinite list" $
        take 10 (takeEvery 1 [1..]) `shouldBe` take 10 [1..10]

    it "n = 3, empty list" $
        takeEvery 3 empty `shouldBe` []
    it "n = 3, finite list" $
        takeEvery 3 [1..10] `shouldBe` [3, 6, 9]
    it "n = 3, prefix of infinite list" $
        take 10 (takeEvery 3 [1..]) `shouldBe` take 10 [3, 6..]

takeUntilSpec :: Spec
takeUntilSpec = do
    it "empty list, undefined function" $
        takeUntil undefined empty `shouldBe` empty
    it "singleton list, undefined function" $
        length (takeUntil undefined singletonUndef) `shouldBe` 1
    it "finite list, undefined function" $
        head (takeUntil undefined [1, 2, 3]) `shouldBe` 1
    it "finite list" $
        takeUntil (>= 5) [1..10] `shouldBe` [1..5]
    it "finite list, always True" $
        takeUntil (const True) [1..10] `shouldBe` [1]
    it "finite list, always False" $
        takeUntil (const False) [1..10] `shouldBe` [1..10]
    it "infinite list" $
        takeUntil (>= 5) [1..] `shouldBe` [1..5]

dropUntilSpec :: Spec
dropUntilSpec = do
    it "empty list, undefined function" $
        dropUntil undefined empty `shouldBe` empty
    it "finite list" $
        dropUntil (== 5) [1..10] `shouldBe` [5..10]
    it "finite list, always True" $
        dropUntil (const True) [1..10] `shouldBe` [1..10]
    it "finite list, always False" $
        dropUntil (const False) [1..10] `shouldBe` []
    it "infinite list" $
        take 6 (dropUntil (== 5) [1..]) `shouldBe` [5..10]

groupSpec :: Spec
groupSpec = do
    it "empty list" $
        group empty `shouldBe` []
    it "singleton list" $ do
        let xss = group singletonUndef
        length xss `shouldBe` 1
        length (head xss) `shouldBe` 1
    it "finite list" $
        group [1, 3, 2, 3, 2, 3] `shouldBe` [[1], [2, 2], [3, 3, 3]]

    let valid :: (Ord a, Show a) => [a] -> Bool
        valid xss = not (any null gs)
                 && all allEqual gs
                 && concat gs == sort xss
                 && allUnique hs
          where
            gs = group xss
            hs = map head gs

    it "arbitrary finite lists" $
        forAll (listOf $ choose (1 :: Int, 10)) (`shouldSatisfy` valid)

groupBySpec :: Spec
groupBySpec = do
    it "empty list" $
        groupBy undefined empty `shouldBe` []
    it "singleton list" $ do
        let xss = groupBy undefined singletonUndef
        length xss `shouldBe` 1
        length (head xss) `shouldBe` 1
    it "finite list" $
        groupBy (comparing Down) [1, 3, 2, 3, 2, 3]
            `shouldBe` [[3, 3, 3], [2, 2], [1]]

groupAdjSpec :: Spec
groupAdjSpec = do
    it "empty list" $
        groupAdj empty `shouldBe` []
    it "singleton list" $ do
        let xss = groupAdj singletonUndef
        length xss `shouldBe` 1
        length (head xss) `shouldBe` 1
    it "finite list" $
        groupAdj [1, 3, 3, 3, 2, 2] `shouldBe` [[1], [3, 3, 3], [2, 2]]
    it "infinite list" $ do
        let -- output == [[1], [2, 2], [3, 3, 3]..]
            output = map (\x -> replicate x x) [1..]
            input = concat output
            n = floor $ sqrt $ fromIntegral infiniteListTruncationLength
        take n (groupAdj input) `shouldBe` take n output

    let valid :: (Eq a, Show a) => [a] -> Bool
        valid xss = not (any null gs)
                 && all allEqual gs
                 && trunc (concat gs) == trunc xss
                 && allAdjUnique hs
          where
            gs = trunc $ groupAdj xss
            hs = map head gs

        test :: (Eq a, Show a) => Gen [a] -> Property
        test gen = forAll gen (`shouldSatisfy` valid)

    it "arbitrary finite lists" $
        test $ sortedGenWith defaultConfig {repeatedness = Repeated}
    it "arbitrary infinite lists" $
        test $ sortedGenWith defaultConfig { repeatedness = Repeated
                                           , finiteness = Infinite}

groupAdjBySpec :: Spec
groupAdjBySpec = do
    it "empty list" $
        groupAdjBy undefined empty `shouldBe` []
    it "singleton list" $ do
        let xss = groupAdjBy undefined singletonUndef
        length xss `shouldBe` 1
        length (head xss) `shouldBe` 1
    it "finite list" $ do
        let eq = (==) `on` head
            input = [ "apple", "at", "atom"
                    , "banana", "bot"
                    , "cat", "curry", "clip"]
            output = [ ["apple", "at", "atom"]
                     , ["banana", "bot"]
                     , ["cat", "curry", "clip"]
                     ]
        groupAdjBy eq input `shouldBe` output

deleteDupsSpec :: Spec
deleteDupsSpec = do
    it "empty list" $
        deleteDups empty `shouldBe` empty
    it "singleton list" $
        length (deleteDups singletonUndef) `shouldBe` 1
    it "arbitrary finite lists" $
        forAll (listOf (choose (1 :: Int, 10))) $ \xs ->
            deleteDups xs === sort (nub xs)

deleteDupsBySpec :: Spec
deleteDupsBySpec = do
    it "empty list" $
        deleteDupsBy undefined empty `shouldBe` empty
    it "singleton list" $
        length (deleteDupsBy undefined singletonUndef) `shouldBe` 1
    it "finite list" $ do
        let cmp = comparing head
        deleteDupsBy cmp ["apple", "banana", "ant", "car", "chest", "boat"]
            `shouldBe` ["apple", "banana", "car"]

deleteAdjDupsSpec :: Spec
deleteAdjDupsSpec = do
    it "empty list" $
        deleteAdjDups empty `shouldBe` empty
    it "singleton list" $
        length (deleteAdjDups singletonUndef) `shouldBe` 1
    it "arbitrary finite lists" $ do
        let gen = sortedGenWith defaultConfig {repeatedness = Repeated}
        forAll gen $ \xs -> deleteAdjDups xs === sort (nub xs)
    it "arbitrary infinite lists" $ do
        let gen = sortedGenWith defaultConfig { repeatedness = Repeated
                                              , finiteness = Infinite}
        forAll gen $ \xs ->
            let ys = trunc $ deleteAdjDups xs
                maxY = last ys
                naive = sort $ nub $ takeWhile (<= maxY) xs
             in ys === naive

deleteAdjDupsBySpec :: Spec
deleteAdjDupsBySpec = do
    it "empty list" $
        deleteAdjDupsBy undefined empty `shouldBe` empty
    it "singleton list" $
        length (deleteAdjDupsBy undefined singletonUndef) `shouldBe` 1
    it "finite list" $ do
        let eq = (==) `on` fst
        deleteAdjDupsBy eq [("a", 3), ("b", 4), ("b", 2), ("c", 4), ("a", 2)]
            `shouldBe` [("a", 3), ("b", 4), ("c", 4), ("a", 2)]

rotateSpec :: Spec
rotateSpec = do
    it "empty list, 0 offset" $
        rotate 0 [] `shouldBe` ([] :: [()])
    it "empty list, positive offset" $
        rotate 3 [] `shouldBe` ([] :: [()])
    it "empty list, negative offset" $
        rotate (-2) [] `shouldBe` ([] :: [()])

    it "singleton list, 0 offset" $
        rotate 0 [1] `shouldBe` [1]
    it "singleton list, positive offset" $
        rotate 2 [1] `shouldBe` [1]
    it "singleton list, negative offset" $
        rotate (-2) [1] `shouldBe` [1]

    let xs = [1..6]
    it "finite list, 0 offset" $
        rotate 0 xs `shouldBe` xs
    it "finite list, positive offset" $
        rotate 2 xs `shouldBe` [3, 4, 5, 6, 1, 2]
    it "finite list, negative offset" $
        rotate (-2) xs `shouldBe` [5, 6, 1, 2, 3, 4]

    it "finite list, positive offset equals length" $
        rotate 6 xs `shouldBe` xs
    it "finite list, negative offset equals negative length" $
        rotate (-6) xs `shouldBe` xs

    it "finite list, positive offset greater than length" $
        rotate 10 xs `shouldBe` [5, 6, 1, 2, 3, 4]
    it "finite list, negative offset greater than length" $
        rotate (-10) xs `shouldBe` [3, 4, 5, 6, 1, 2]

    it "infinite list, 0 offset" $
        take 10 (rotate 0 [1..]) `shouldBe` [1..10]
    it "ininite list, positive offset" $
        take 10 (rotate 10 [1..]) `shouldBe` [11..20]

    let bigOffset = 3*10^8
    it "finite list, huge positive offset" $
        rotate bigOffset xs `shouldBe` rotate (bigOffset `mod` length xs) xs
    it "finite list, huge negative offset" $
        rotate (-bigOffset) xs
            `shouldBe` rotate ((-bigOffset) `mod` length xs) xs
