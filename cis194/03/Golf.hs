module Golf where

import Control.Arrow ((&&&))
import Data.List
import Data.Monoid
import Data.Ord (comparing)

{----------------------------------------------------------------------------------------------------{
                                                                      | Exercise 1
}----------------------------------------------------------------------------------------------------}

skips :: [a] -> [[a]]
skips xs = zipWith nth [1..length xs] $ repeat xs

nth :: Int -> [a] -> [a]
nth i xs
	| i < 2 = xs
	| length xs < i = []
	| otherwise = xs !! (i - 1) : nth i (drop i xs)

{----------------------------------------------------------------------------------------------------{
                                                                      | Exercise 2
}----------------------------------------------------------------------------------------------------}

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs)
	| x < y && y > z = y : xs'
	| otherwise = xs'
	where xs' = localMaxima $ y : z : xs
localMaxima _ = []

{----------------------------------------------------------------------------------------------------{
                                                                      | Exercise 3
}----------------------------------------------------------------------------------------------------}

histogram :: [Integer] -> String
histogram xs = s ++ unlines [replicate 10 '=', ['0'..'9']]
	where
		-- known numbers and their quantity
		xs' :: [(Integer, Int)]
		xs' = map (head &&& length) $ groupBy (==) $ sort xs

		-- missing numbers
		m :: [(Integer, Int)]
		m = zip ([0..9] \\ xs) (repeat 0)

		-- ditch the tuple and just give us the number counts
		c :: [Int]
		c = map snd $ sortBy (comparing fst) $ xs' ++ m
		-- note that `sortBy (comparing fst)` should be comparable to `sortOn fst`...
		-- but sortOn wasn't available in my version of GHC

		-- visual representation of the stars portion of the histogram
		s :: String
		s = unlines $ reverse $ rowStars c

rowStars :: (Num a, Eq a) => [a] -> [String]
rowStars xs
	| sum xs == 0 = []
	| otherwise = map fst x : rowStars (map snd x)
	where
		x = map numToStar xs

numToStar :: (Num a, Eq a) => a -> (Char, a)
numToStar 0 = (' ', 0)
numToStar x = ('*', x - 1)