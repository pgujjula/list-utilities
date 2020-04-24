module Data.List.PredicateSpec where


import Data.Function (on)
import Data.Ord (Down(Down), comparing)

import Test.Hspec            (Spec, describe, it, shouldBe, hspec, Expectation)
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck (Gen, arbitrary, oneof, listOf, suchThat, forAll)

import Data.List.Predicate   (allEqual, allEqualBy, sorted, sortedBy, allAdjUnique,
                              allAdjUniqueBy, allUniqueBy, allUnique, palindrome,
                              ascSequential, descSequential)

unexp :: Expectation
unexp = undefined
-- TODO: Remove if not needed
numTests :: Int
numTests = 1000

spec :: Spec
spec = modifyMaxSuccess (const numTests) $ do
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
    allEqual ([] :: [Int]) `shouldBe` True
  it "singleton" $
    allEqual ([3] :: [Int]) `shouldBe` True
  it "repeated element, finite list" $
    allEqual (replicate 10 (3 :: Int)) `shouldBe` True
  it "single unequal element, finite list" $
    (allEqual $ (replicate 10 (3 :: Int)) ++ [4] ++ (replicate 10 3)) `shouldBe` False
  it "single unequal element, infinite list" $ 
    (allEqual $ (replicate 10 (3 :: Int)) ++ [4] ++ (repeat 3)) `shouldBe` False

allEqualBySpec :: Spec
allEqualBySpec = do
  it "empty list" $ 
    allEqualBy undefined ([] :: [Int]) `shouldBe` True
  it "singleton" $
    allEqualBy undefined ([3] :: [Int]) `shouldBe` True

  let eq = (==) `on` (`rem` (10 :: Int))
  it "repeated element, finite list" $
    allEqualBy eq [3, 13, 23 :: Int] `shouldBe` True
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
    sorted [2, 4..10 :: Int] `shouldBe` True
  it "finite list, not sorted" $
    sorted ([2, 4..10 :: Int] ++ [9]) `shouldBe` False
  it "finite list, sorted, some repeats" $
    sorted ([1, 2, 5, 5, 6, 9, 9, 9] :: [Int]) `shouldBe` True
  it "finite list, not sorted, some repeats" $
    sorted ([1, 2, 2, 3, 6, 5, 7, 7, 9] :: [Int]) `shouldBe` False
  it "infinite list, not sorted" $
    sorted ([2, 4..10 :: Int] ++ [9] ++ [11..]) `shouldBe` False
  it "infinite list, not sorted, some repeats" $
    sorted ([1, 2, 2, 3, 6, 5, 7, 7, 9] ++ [10..] :: [Int]) `shouldBe` False

sortedBySpec :: Spec
sortedBySpec = do
  let cmp :: (Ord a) => a -> a -> Ordering
      cmp = comparing Down
   in do
      it "empty" $
        sortedBy undefined ([] :: [Int]) `shouldBe` True
      it "singleton" $
        sortedBy undefined [3 :: Int] `shouldBe` True
      it "finite list, sorted" $
        sortedBy cmp [10, 8..2 :: Int] `shouldBe` True
      it "finite list, not sorted" $
        sortedBy cmp ([10, 8..2 :: Int] ++ [3]) `shouldBe` False
      it "finite list, sorted, some repeats" $
        sortedBy cmp ([9, 7, 4, 4, 3, 1, 1, 1] :: [Int]) `shouldBe` True
      it "finite list, not sorted, some repeats" $
        sortedBy cmp ([9, 8, 8, 7, 4, 5, 3, 3, 1] :: [Int]) `shouldBe` False
      it "infinite list, not sorted" $
        sorted ([10, 8..2 :: Int] ++ [3] ++ [1, 0..]) `shouldBe` False
      it "infinite list, not sorted, some repeats" $
        sortedBy cmp ([9, 8, 8, 7, 4, 5, 3, 3, 1] ++ [0, -1..] :: [Int]) `shouldBe` False

allUniqueSpec :: Spec
allUniqueSpec = do
  it "empty list" $
    allUnique ([] :: [Int]) `shouldBe` True
  it "singleton list" $
    allUnique [1 :: Int] `shouldBe` True
  it "finite list, no repeats" $
    allUnique [10, 9..1 :: Int] `shouldBe` True
  it "finite list, one repeat" $
    allUnique (1:[10, 9..1 :: Int]) `shouldBe` False
  it "finite list, two repeats" $
    allUnique (1:2:[10, 9..1 :: Int]) `shouldBe` False

allUniqueBySpec :: Spec
allUniqueBySpec = do
  it "empty list" $ 
    allUniqueBy undefined [] `shouldBe` True
  it "singleton list" $
    allUniqueBy undefined [1 :: Int] `shouldBe` True
  it "finite list, no repeats" $
    allUniqueBy (comparing (`rem` 10)) [1..10] `shouldBe` True
  it "finite list, one repeat" $
    allUniqueBy (comparing (`rem` 10)) [1..11] `shouldBe` False

allAdjUniqueSpec :: Spec
allAdjUniqueSpec = do
  it "empty list" $ 
    allAdjUnique ([] :: [Int]) `shouldBe` True
  it "singleton list" $ 
    allAdjUnique ([1] :: [Int]) `shouldBe` True
  it "finite list, no repeats" $ 
    allAdjUnique ([1, 5, 2, 8, 2, 5] :: [Int]) `shouldBe` True
  it "finite list, one repeat" $ 
    allAdjUnique ([1, 5, 5, 8, 2, 5] :: [Int]) `shouldBe` False
  it "finite list, two repeats" $ 
    allAdjUnique ([1, 5, 5, 8, 2, 2, 5] :: [Int]) `shouldBe` False
  it "infinite list, one repeat" $ 
    allAdjUnique ([1, 2, 3, 3] ++ [4..]) `shouldBe` False
  it "infinite list, two repeat" $ 
    allAdjUnique ([1, 2, 3, 3] ++ [4, 5, 6, 6] ++ [0, -1..]) `shouldBe` False

allAdjUniqueBySpec :: Spec
allAdjUniqueBySpec = do
  it "empty list" $ 
    allAdjUniqueBy undefined ([] :: [Int]) `shouldBe` True
  it "singleton list" $ 
    allAdjUniqueBy undefined ([1] :: [Int]) `shouldBe` True
  it "finite list, no repeats" $ 
    allAdjUniqueBy ((==) `on` (`rem` 10)) ([1, 5, 19, 8, 2, 5] :: [Int]) `shouldBe` True
  it "finite list, one repeat" $ 
    allAdjUniqueBy ((==) `on` (`rem` 10)) ([1, 5, 18, 8, 2, 5] :: [Int]) `shouldBe` False

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

  let naive xs = and $ zipWith (==) xs (reverse xs)
  let palindromeGen = do
        xs <- listOf arbitrary :: Gen String
        c  <- oneof [return "", (:[]) <$> arbitrary] :: Gen String -- either a character or nothing
        return $ xs ++ c ++ (reverse xs)
  let nonPalindromeGen = listOf arbitrary `suchThat` (not . naive) :: Gen String
  it "arbitrary palindromes" $ do
    forAll palindromeGen palindrome
  it "arbitrary non-palindromes" $ do
    forAll nonPalindromeGen (not . palindrome)

ascSequentialSpec :: Spec
ascSequentialSpec = do
  it "empty list" $ 
    ascSequential ([] :: [Int]) `shouldBe` True
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
    descSequential ([10, 9..6] ++ [6] ++ [undefined] ++ [5, 4..1]) `shouldBe` False
    descSequential ([10, 9..6] ++ [7] ++ [undefined] ++ [5, 4..1]) `shouldBe` False
  it "infinite list, not ascending" $
    descSequential ([0, -1..(-10)] ++ [-9] ++ [undefined] ++ [-9, -8..]) `shouldBe` False
