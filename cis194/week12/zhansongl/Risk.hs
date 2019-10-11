{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad (replicateM)
import Data.List (sortBy)
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
successProb b = (/) <$> (fromIntegral <$> successCount) <*> pure 1000
  where rs = replicateM 1000 . invade $ b
        successCount = (length . filter winning) <$> rs
        winning (Battlefield a _) = a > 2
