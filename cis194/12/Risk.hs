{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.Functor
import Data.List

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

data Battlefield = Battlefield { attackers :: Army, defenders :: Army } deriving (Show)

{----------------------------------------------------------------------------------------------------{
                                                                      | Exercise 2
}----------------------------------------------------------------------------------------------------}

dice :: Int -> Rand StdGen [DieValue]
dice n = sequence (replicate n die)

battle :: Battlefield -> Rand StdGen Battlefield
battle b = do
	attRoll <- roll 3 $ attackers b - 1
	defRoll <- roll 2 $ defenders b
	return $ if maximum attRoll > maximum defRoll
		then b { defenders = defenders b - 1 }
		else b { attackers = attackers b - 1 }
	where
		roll maxSize size = dice $ min maxSize size

{----------------------------------------------------------------------------------------------------{
                                                                      | Exercise 3
}----------------------------------------------------------------------------------------------------}

invade :: Battlefield -> Rand StdGen Battlefield
invade b@(Battlefield a d)
	| a < 2 || d == 0 = return b
	| otherwise = battle b >>= invade

{----------------------------------------------------------------------------------------------------{
                                                                      | Exercise 4
}----------------------------------------------------------------------------------------------------}

successProb :: Battlefield -> Rand StdGen Double
successProb b = prob <$> sequence (replicate 1000 $ invade b)
	where
		prob r = f r / 1000
		f = fromIntegral . length . filter ((0 ==) . defenders)

{----------------------------------------------------------------------------------------------------{
                                                                      | Exercise 5 (optional)
}----------------------------------------------------------------------------------------------------}

exactSuccessProb :: Battlefield -> Double
exactSuccessProb = undefined
