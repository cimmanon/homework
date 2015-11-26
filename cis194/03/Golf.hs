module Golf where

import Data.List
import Data.Monoid

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
		-- how many times the numbers 0-9 appear in the list
		xs' :: [Int]
		xs' = f [0..9] $ groupBy (==) $ sort xs

		-- visual representation of the stars portion of the histogram
		s :: String
		s = unlines $ reverse $ rowStars xs'

-- function takes a list of things we want to count
-- plus a list of sorted & grouped things (possibly with some missing)
-- returns how many times each element from the first list appears in the second
-- note that this will fail if the 2nd list contains empty lists like [[1], [], [5, 5]]
-- handling this case would require extra code...
f :: Eq a => [a] -> [[a]] -> [Int]
f (x:xs) (x2@(y:_):xs2)
	| x == y = length x2 : f xs xs2
	| otherwise = 0 : f xs (x2:xs2)
f xs _ = replicate (length xs) 0

rowStars :: (Num a, Eq a) => [a] -> [String]
rowStars xs
	| sum xs == 0 = []
	| otherwise = map fst x : rowStars (map snd x)
	where
		x = map numToStar xs

numToStar :: (Num a, Eq a) => a -> (Char, a)
numToStar 0 = (' ', 0)
numToStar x = ('*', x - 1)