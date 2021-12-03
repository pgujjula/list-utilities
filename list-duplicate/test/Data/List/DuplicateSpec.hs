module Data.List.DuplicateSpec (spec) where

import Data.List.Duplicate (deleteAdjDups, deleteAdjDupsBy, deleteDups,
                            deleteDupsBy, group, groupAdj, groupAdjBy, groupBy,
                            mode, modeBy)

import Data.Function       (on)
import Data.List           (nub, sort)
import Data.Ord            (Down (Down), comparing)
import Test.Hspec          (Spec, describe, it, shouldBe)
import Test.QuickCheck     (choose, forAll, listOf, (===))

infiniteListTruncationLength :: Int
infiniteListTruncationLength = 100

empty :: [Integer]
empty = []

singletonUndef :: [Integer]
singletonUndef = [undefined]

spec :: Spec
spec = do
    describe "group" groupSpec
    describe "groupBy" groupBySpec
    describe "groupAdj" groupAdjSpec
    describe "groupAdjBy" groupAdjBySpec

    describe "deleteDups" deleteDupsSpec
    describe "deleteDupsBy" deleteDupsBySpec
    describe "deleteAdjDups" deleteAdjDupsSpec
    describe "deleteAdjDupsBy" deleteAdjDupsBySpec

    describe "mode" modeSpec
    describe "modeBy" modeBySpec

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

--    let valid :: (Ord a, Show a) => [a] -> Bool
--        valid xss = not (any null gs)
--                 && all allEqual gs
--                 && concat gs == sort xss
--                 && allUnique hs
--          where
--            gs = group xss
--            hs = map head gs
--
--    it "arbitrary finite lists" $
--        forAll (listOf $ choose (1 :: Int, 10)) (`shouldSatisfy` valid)

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

--    let valid :: (Eq a, Show a) => [a] -> Bool
--        valid xss = not (any null gs)
--                 && all allEqual gs
--                 && trunc (concat gs) == trunc xss
--                 && allAdjUnique hs
--          where
--            gs = trunc $ groupAdj xss
--            hs = map head gs
--
--        test :: (Eq a, Show a) => Gen [a] -> Property
--        test gen = forAll gen (`shouldSatisfy` valid)
--
--    it "arbitrary finite lists" $
--        test $ sortedGenWith defaultConfig {repeatedness = Repeated}
--    it "arbitrary infinite lists" $
--        test $ sortedGenWith defaultConfig { repeatedness = Repeated
--                                           , finiteness = Infinite}

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
--    it "arbitrary finite lists" $ do
--        let gen = sortedGenWith defaultConfig {repeatedness = Repeated}
--        forAll gen $ \xs -> deleteAdjDups xs === sort (nub xs)
--    it "arbitrary infinite lists" $ do
--        let gen = sortedGenWith defaultConfig { repeatedness = Repeated
--                                              , finiteness = Infinite}
--        forAll gen $ \xs ->
--            let ys = trunc $ deleteAdjDups xs
--                maxY = last ys
--                naive = sort $ nub $ takeWhile (<= maxY) xs
--             in ys === naive

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

modeSpec :: Spec
modeSpec = do
  it "empty list" $
      mode ([] :: [Int]) `shouldBe` Nothing
  it "singleton list" $
      mode ([1] :: [Int]) `shouldBe` Just 1
  it "finite list" $
      mode ([1,1,2,1,2,0] :: [Int]) `shouldBe` Just 1

modeBySpec :: Spec
modeBySpec = do
  it "empty list" $
      modeBy undefined ([] :: [Int]) `shouldBe` Nothing
  it "singleton list" $
      modeBy undefined ([1] :: [Int]) `shouldBe` Just 1
  it "finite list" $
      fmap even (modeBy (comparing even) ([1,1,2,1,2,0] :: [Int]))
          `shouldBe` Just True
