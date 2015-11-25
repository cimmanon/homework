module CC where

{----------------------------------------------------------------------------------------------------{
                                                                      | Exercise 1
}----------------------------------------------------------------------------------------------------}

{-
-- This version also works, but it's probably not the desired solution

toDigits :: Integer -> [Integer]
toDigits = map (read . (:[])) . show
-}

toDigits :: Integer -> [Integer]
toDigits x
	| x < 1 = []
	| otherwise = toDigits (x `div` 10) ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- this is more efficient than concatenating lists together, but we get the digits in reverse order
-- note that this is what we actually want anyway...
toDigitsRev' :: Integer -> [Integer]
toDigitsRev' x
	| x < 1 = []
	| otherwise = x `mod` 10 : toDigitsRev' (x `div` 10)

{----------------------------------------------------------------------------------------------------{
                                                                      | Exercise 2
}----------------------------------------------------------------------------------------------------}

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = zipWith ($) (cycle [id, (* 2)])

{----------------------------------------------------------------------------------------------------{
                                                                      | Exercise 3
}----------------------------------------------------------------------------------------------------}

sumDigits :: [Integer] -> Integer
sumDigits = sum . map check
	where
		check x
			| x > 10 = sum $ toDigits x
			| otherwise = x

{----------------------------------------------------------------------------------------------------{
                                                                      | Exercise 4
}----------------------------------------------------------------------------------------------------}

validate :: Integer -> Bool
validate = isValid . sumDigits . doubleEveryOther . toDigitsRev'
	where
		isValid x = x `rem` 10 == 0
