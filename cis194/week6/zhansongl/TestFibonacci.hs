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

  describe "fibs2" $ do
    it "should have [0,1] as the first two elements" $ do
      take 2 fibs2 `shouldBe` [0,1]
    it "should generate correct fibanocci sequence" $ property $
      forAll (arbitrary :: Gen (Small Int))
        (\x -> let n = getSmall x
                in isFibonacciSequence $ take n fibs2)

isFibonacciSequence :: [Integer] -> Bool
isFibonacciSequence [] = True
isFibonacciSequence [x] = True
isFibonacciSequence [x,y] = True
isFibonacciSequence (x:y:z:zs) = z == (x+y) && isFibonacciSequence (y:z:zs)

