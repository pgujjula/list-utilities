module Data.List.PredicateSpec where

import Data.Function (on)
import Data.Ord (Down(Down), comparing)

import Test.Hspec            (Spec, describe, it, shouldBe)
import Test.QuickCheck (Gen, arbitrary, oneof, listOf, suchThat, forAll)

import Data.List.Predicate   (allEqual, allEqualBy, sorted, sortedBy, allAdjUnique,
                              allAdjUniqueBy, allUniqueBy, allUnique, palindrome,
                              ascSequential, descSequential)

spec :: Spec
spec = do
    describe "allEqual" allEqualSpec
    describe "allEqualBy" allEqualBySpec
    describe "sorted" sortedSpec
    describe "sortedBy" sortedBySpec
    describe "allUnique" allAdjUniqueSpec
    describe "allUniqueBy" allAdjUniqueBySpec
    describe "allAdjUnique" allAdjUniqueSpec
    describe "allAdjUniqueBy" allAdjUniqueBySpec
    describe "ascSequentialSpec" ascSequentialSpec
    describe "descSequentialSpec" descSequentialSpec
    describe "palindrome" palindromeSpec

allEqualSpec :: Spec
allEqualSpec = do
    it "empty list" $ 
        allEqual ([] :: [()]) `shouldBe` True
    it "singleton" $
        allEqual [3] `shouldBe` True
    it "repeated element, finite list" $
        allEqual (replicate 10 3) `shouldBe` True
    it "single unequal element, finite list" $ do
        let xs = (replicate 10 3) ++ [4] ++ (replicate 10 3)
        allEqual xs `shouldBe` False
    it "single unequal element, infinite list" $ do
        let xs = (replicate 10 3) ++ [4] ++ (repeat 3)
        allEqual xs `shouldBe` False

allEqualBySpec :: Spec
allEqualBySpec = do
    it "empty list" $ 
        allEqualBy undefined ([] :: [()]) `shouldBe` True
    it "singleton" $
        allEqualBy undefined [3] `shouldBe` True

    let eq = (==) `on` (`rem` 10)
    it "repeated element, finite list" $
        allEqualBy eq [3, 13, 23] `shouldBe` True
    it "single unequal element, finite list" $ do
        let xs = [3, 13, 23, 34, 43]
        allEqualBy eq xs `shouldBe` False
    it "single unequal element, infinite list" $ do
        let xs = [3, 13, 23, 34] ++ [43, 53..]
        allEqualBy eq xs `shouldBe` False

sortedSpec :: Spec
sortedSpec = do
    it "empty" $ sorted ([] :: [Int]) `shouldBe` True
    it "singleton" $ sorted [3 :: Int] `shouldBe` True
    it "finite list, sorted" $
        sorted [2, 4..10] `shouldBe` True
    it "finite list, not sorted" $
        sorted ([2, 4..10] ++ [9]) `shouldBe` False
    it "finite list, sorted, some repeats" $
        sorted [1, 2, 5, 5, 6, 9, 9, 9] `shouldBe` True
    it "finite list, not sorted, some repeats" $
        sorted [1, 2, 2, 3, 6, 5, 7, 7, 9] `shouldBe` False
    it "infinite list, not sorted" $
        sorted ([2, 4..10] ++ [9] ++ [11..]) `shouldBe` False
    it "infinite list, not sorted, some repeats" $
        sorted ([1, 2, 2, 3, 6, 5, 7, 7, 9] ++ [10..]) `shouldBe` False

sortedBySpec :: Spec
sortedBySpec = do
    let cmp :: (Ord a) => a -> a -> Ordering
        cmp = comparing Down
     in do
        it "empty" $
            sortedBy undefined ([] :: [()]) `shouldBe` True
        it "singleton" $
            sortedBy undefined [3] `shouldBe` True
        it "finite list, sorted" $
            sortedBy cmp [10, 8..2] `shouldBe` True
        it "finite list, not sorted" $
            sortedBy cmp ([10, 8..2] ++ [3]) `shouldBe` False
        it "finite list, sorted, some repeats" $
            sortedBy cmp [9, 7, 4, 4, 3, 1, 1, 1] `shouldBe` True
        it "finite list, not sorted, some repeats" $
            sortedBy cmp [9, 8, 8, 7, 4, 5, 3, 3, 1] `shouldBe` False
        it "infinite list, not sorted" $
            sorted ([10, 8..2] ++ [3] ++ [1, 0..]) `shouldBe` False
        it "infinite list, not sorted, some repeats" $
            sortedBy cmp ([9, 8, 8, 7, 4, 5, 3, 3, 1] ++ [0, -1..])
                `shouldBe` False

allUniqueSpec :: Spec
allUniqueSpec = do
    it "empty list" $
        allUnique ([] :: [()]) `shouldBe` True
    it "singleton list" $
        allUnique [1] `shouldBe` True
    it "finite list, no repeats" $
        allUnique [10, 9..1] `shouldBe` True
    it "finite list, one repeat" $
        allUnique (1:[10, 9..1]) `shouldBe` False
    it "finite list, two repeats" $
        allUnique (1:2:[10, 9..1]) `shouldBe` False

allUniqueBySpec :: Spec
allUniqueBySpec = do
    it "empty list" $ 
        allUniqueBy undefined [] `shouldBe` True
    it "singleton list" $
        allUniqueBy undefined [1] `shouldBe` True

    let cmp = comparing (`rem` 10)
    it "finite list, no repeats" $
        allUniqueBy cmp [1..10] `shouldBe` True
    it "finite list, one repeat" $
        allUniqueBy cmp [1..11] `shouldBe` False

allAdjUniqueSpec :: Spec
allAdjUniqueSpec = do
    it "empty list" $ 
        allAdjUnique ([] :: [()]) `shouldBe` True
    it "singleton list" $ 
        allAdjUnique [1] `shouldBe` True
    it "finite list, no repeats" $ 
        allAdjUnique [1, 5, 2, 8, 2, 5] `shouldBe` True
    it "finite list, one repeat" $ 
        allAdjUnique [1, 5, 5, 8, 2, 5] `shouldBe` False
    it "finite list, two repeats" $ 
        allAdjUnique [1, 5, 5, 8, 2, 2, 5] `shouldBe` False
    it "infinite list, one repeat" $ 
       allAdjUnique ([1, 2, 3, 3] ++ [4..]) `shouldBe` False
    it "infinite list, two repeat" $ 
        allAdjUnique ([1, 2, 3, 3] ++ [4, 5, 6, 6] ++ [0, -1..])
            `shouldBe` False

allAdjUniqueBySpec :: Spec
allAdjUniqueBySpec = do
    it "empty list" $ 
        allAdjUniqueBy undefined ([] :: [()]) `shouldBe` True
    it "singleton list" $ 
        allAdjUniqueBy undefined [1] `shouldBe` True

    let eq = (==) `on` (`rem` 10)
    it "finite list, no repeats" $ 
        allAdjUniqueBy eq [1, 5, 19, 8, 2, 5] `shouldBe` True
    it "finite list, one repeat" $ 
        allAdjUniqueBy eq [1, 5, 18, 8, 2, 5] `shouldBe` False

palindromeSpec :: Spec
palindromeSpec = do
    it "empty list" $ 
        palindrome "" `shouldBe` True
    it "singleton list" $ 
        palindrome "a" `shouldBe` True
    it "small palindrome, odd length" $ 
        palindrome "rotor" `shouldBe` True
    it "small not palindrome, odd length" $
        palindrome "rover" `shouldBe` False
    it "small palindrome, even length" $ 
        palindrome "dood" `shouldBe` True
    it "small not palindrome, even length" $
        palindrome "door" `shouldBe` False

    let naive :: (Eq a) => [a] -> Bool
        naive xs = and $ zipWith (==) xs (reverse xs)

        palindromeGen :: Gen String
        palindromeGen = do
            xs <- listOf arbitrary
            -- c is either a character or nothing
            c  <- oneof [return "", (:[]) <$> arbitrary]
            return $ xs ++ c ++ (reverse xs)

        nonPalindromeGen :: Gen String
        nonPalindromeGen = listOf arbitrary `suchThat` (not . naive) :: Gen String
     in do
        it "arbitrary palindromes" $
            forAll palindromeGen palindrome
        it "arbitrary non-palindromes" $
            forAll nonPalindromeGen (not . palindrome)

ascSequentialSpec :: Spec
ascSequentialSpec = do
    it "empty list" $ 
        ascSequential ([] :: [()]) `shouldBe` True
    it "singleton list" $
        ascSequential [1] `shouldBe` True
    it "finite list, ascending" $
        ascSequential [1..10] `shouldBe` True
    it "finite list, not ascending" $ do
        ascSequential ([1..5] ++ [5] ++ [undefined] ++ [6..10]) `shouldBe` False
        ascSequential ([1..5] ++ [4] ++ [undefined] ++ [6..10]) `shouldBe` False
    it "infinite list, not ascending" $
        ascSequential ([1..10] ++ [9] ++ [undefined] ++ [9..]) `shouldBe` False

descSequentialSpec :: Spec
descSequentialSpec = do
    it "empty list" $
        descSequential ([] :: [Int]) `shouldBe` True
    it "singleton list" $
        descSequential ([] :: [Int]) `shouldBe` True
    it "finite list, descending" $
        descSequential [10, 9..1] `shouldBe` True
    it "finite list, not ascending" $ do
        descSequential ([10, 9..6] ++ [6] ++ [undefined] ++ [5, 4..1])
            `shouldBe` False
        descSequential ([10, 9..6] ++ [7] ++ [undefined] ++ [5, 4..1])
            `shouldBe` False
    it "infinite list, not ascending" $
        descSequential ([0, -1..(-10)] ++ [-9] ++ [undefined] ++ [-9, -8..])
            `shouldBe` False
