{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad (replicateM)
import Data.List (sortBy, sort, group)
--import Debug.Trace (trace)
import System.Random (StdGen)

-- monadrandom
import Control.Monad.Random (MonadRandom(..), Random(..), Rand)
-- monad-loops
import Control.Monad.Loops (iterateUntilM)

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving (Show, Eq)

battleOutcome :: Battlefield -> [DieValue] -> [DieValue] -> Battlefield
battleOutcome field attackRolls defendRolls = Battlefield attackOutcome defendOutcome
  where attackOutcome = attackers field - (length . filter not $ result)
        defendOutcome = defenders field - (length . filter id $ result)
        result = zipWith (>) attackRollsSorted defendRollsSorted
        attackRollsSorted = sortBy (flip compare) attackRolls
        defendRollsSorted = sortBy (flip compare) defendRolls

battle :: Battlefield -> Rand StdGen Battlefield
battle field
  | (attackers field <= 1 || defenders field <= 0) = pure field
  | otherwise = replicateM attackArmy die >>= \attackRolls ->
                replicateM defendArmy die >>= \defendRolls ->
                pure $ battleOutcome field attackRolls defendRolls
      where attackArmy = min 3 (attackers field - 1)
            defendArmy = min 2 (defenders field)

invade :: Battlefield -> Rand StdGen Battlefield
invade = iterateUntilM battleOver battle
  where battleOver (Battlefield a d) = a <= 2 || d <= 0

successProb :: Battlefield -> Rand StdGen Double
successProb b = (/) <$> (fromIntegral <$> successCount) <*> pure 10000
  where rs = replicateM 10000 . invade $ b
        successCount = (length . filter winning) <$> rs
        winning (Battlefield a _) = a > 2

cartProd :: [m a] -> [m a] -> [(m a, m a)]
cartProd o1 o2 = [(x,y) | x <- o1, y <- o2]

outcome :: Int -> [[Int]]
outcome 0 = []
outcome 1 = [[1], [2], [3], [4], [5], [6]]
outcome n = fmap (sortBy $ flip compare) . fmap (uncurry (++)) $ cartProd (outcome 1) (outcome $ n-1)

fightOutcome :: ([Int], [Int]) -> (Int, Int)
fightOutcome (as, ds) = (aloss, dloss)
  where r = zipWith (>) as ds
        dloss = length . filter id $ r
        aloss = length . filter not $ r

outcomeProb :: Int -> Int -> [(Int, Int, Double)]
outcomeProb a d = zipWith p (fmap head . group $ r) (fmap length . group $ r)
  where r = sort . fmap fightOutcome $ cartProd (outcome a) (outcome d)
        denom = length r
        p (aloss, dloss) num = (aloss, dloss, fromIntegral num / fromIntegral denom)

exactSuccessProb :: Battlefield -> Double
exactSuccessProb (Battlefield a d)
  | a <= 2 = 0
  | d <= 0 = 1
  | otherwise = sum
              . fmap (\(aloss, dloss, prob)
                      -> (exactSuccessProb $ Battlefield (a - aloss) (d - dloss)) * prob)
              $ outcomeProb as ds
  where as = min 3 (a-1)
        ds = min 2 d
