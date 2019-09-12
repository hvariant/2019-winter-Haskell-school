import Fibonacci

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "fib" $ do
    it "should satisfy the base properties" $ do
        fib 0 `shouldBe` 0
        fib 1 `shouldBe` 1

    it "should satisfy the recursive property" $ property $
      forAll (choose (0, 20))
        (\n -> fib (n+2) == fib (n+1) + fib n)

  describe "fibs1" $ do
    it "the first few values should be correct" $ do
      take 13 fibs1 `shouldBe` [0,1,1,2,3,5,8,13,21,34,55,89,144]

