import Risk

import Control.Monad.Trans.Random.Lazy (evalRand)
import System.Random (mkStdGen)

import Test.Hspec

main = hspec $ do
  describe "battle" $ do
    it "should do nothing when there isn't enough attackers" $ do
      (battle (Battlefield 0 12) `evalRand` mkStdGen 42) `shouldBe` (Battlefield 0 12)
      (battle (Battlefield 1 12) `evalRand` mkStdGen 42) `shouldBe` (Battlefield 1 12)
    it "should do nothing when there isn't enough defenders" $ do
      (battle (Battlefield 12 0) `evalRand` mkStdGen 42) `shouldBe` (Battlefield 12 0)

    describe "battleOutcome" $ do
      it "should produce the correct outcome given enough attackers and defenders" $ do
        (battleOutcome (Battlefield 3 5) (fmap DV [3,5]) (fmap DV [4,3])) `shouldBe` (Battlefield 2 4)
        (battleOutcome (Battlefield 12 12) (fmap DV [1,4,2]) (fmap DV [3,5])) `shouldBe` (Battlefield 10 12)

  describe "invade" $ do
    it "should battle until either army is depleted" $ do
      (invade (Battlefield 100 10) `evalRand` mkStdGen 42) `shouldSatisfy` \(Battlefield a d) -> d <= 0
      (invade (Battlefield 300 10000) `evalRand` mkStdGen 42) `shouldSatisfy` \(Battlefield a d) -> a <= 1
