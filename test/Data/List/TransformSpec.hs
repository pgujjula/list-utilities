module Data.List.TransformSpec (spec) where

import Test.Hspec (Spec, shouldBe, it)

spec :: Spec
spec = it "test" $ (1 + 1) `shouldBe` (2 :: Int)
