module Scrabble where

import Data.Char
import Data.Monoid

newtype Score = Score Int deriving (Show, Eq)

getScore :: Score -> Int
getScore (Score i) = i

instance Monoid Score where
	mempty = Score 0
	mappend (Score i1) (Score i2) = Score $ i1 + i2

scoreChar :: Char -> Score
scoreChar = Score . score
	where
		score c
			| c `elem` "aeilnorstu" = 1
			| c `elem` "dg" = 2
			| c `elem` "bcmp" = 3
			| c `elem` "fhvwy" = 4
			| c `elem` "k" = 5
			| c `elem` "jx" = 8
			| c `elem` "qz" = 10
			| otherwise = 0

scoreString :: [Char] -> Score
scoreString = foldl (\ a b -> a <> scoreChar (toLower b)) mempty
