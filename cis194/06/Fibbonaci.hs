{-# LANGUAGE FlexibleInstances #-}

module Fibbonaci where

{----------------------------------------------------------------------------------------------------{
                                                                      | Exercise 1
}----------------------------------------------------------------------------------------------------}

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

{----------------------------------------------------------------------------------------------------{
                                                                      | Exercise 2
}----------------------------------------------------------------------------------------------------}

fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)

{----------------------------------------------------------------------------------------------------{
                                                                      | Exercise 3
}----------------------------------------------------------------------------------------------------}

data Stream a = Cons a (Stream a) deriving (Eq)

instance Show a => Show (Stream a) where
	show = show . streamTake 20

streamTake :: Integer -> Stream a -> [a]
streamTake 1 (Cons s _) = [s]
streamTake i (Cons s ss) = s : streamTake (i - 1) ss

{----------------------------------------------------------------------------------------------------{
                                                                      | Exercise 4
}----------------------------------------------------------------------------------------------------}

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons s ss) = Cons (f s) (streamMap f ss)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))

{----------------------------------------------------------------------------------------------------{
                                                                      | Exercise 5
}----------------------------------------------------------------------------------------------------}

zipWithStream :: (a -> a -> b) -> Stream a -> Stream a -> Stream b
zipWithStream f (Cons s1 ss1) (Cons s2 ss2) = Cons (f s1 s2) $ zipWithStream f ss1 ss2

--zipStream :: Stream a -> Stream a -> Stream (a, a)
--zipStream = zipWithStream ( , )

nats :: Stream Integer
nats = streamFromSeed (+1) 0

--powOf2 :: Stream (Integer, Integer)
--powOf2 = zipStream nats $ Cons 0 $ streamFromSeed (* 2) 2

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons s1 ss1) (Cons s2 ss2) = (Cons s1 (Cons s2 (interleaveStreams ss1 ss2)))

{----------------------------------------------------------------------------------------------------{
                                                                      | Exercise 6 (optional)
}----------------------------------------------------------------------------------------------------}

instance Num (Stream Integer) where
	(+) = zipWithStream (+)
	(*) = zipWithStream (*)
	abs = streamMap abs
	signum = streamMap signum
	fromInteger = streamRepeat
	negate = streamMap ( * (-1))

-- TODO

-- fibs3 :: Stream Integer

{----------------------------------------------------------------------------------------------------{
                                                                      | Exercise 7 (optional)
}----------------------------------------------------------------------------------------------------}

-- TODO