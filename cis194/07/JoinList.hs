{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, OverlappingInstances #-}

module JoinList where

import Control.Applicative
import Data.Monoid
import Sized
import Scrabble
import Buffer
import Editor

data JoinList m a
	= Empty
	| Single m a
	| Append m (JoinList m a) (JoinList m a)
	deriving (Eq, Show)

{----------------------------------------------------------------------------------------------------{
                                                                      | Exercise 1
}----------------------------------------------------------------------------------------------------}

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty s = s
(+++) s Empty = s
(+++) j1 j2 = Append (tag j1 <> tag j2) j1 j2

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

{----------------------------------------------------------------------------------------------------{
                                                                      | Exercise 2
}----------------------------------------------------------------------------------------------------}

y :: JoinList Size Char
y = (Single (Size 1) 'x') +++ (Single (Size 1) 'y')

x :: JoinList Size Char
x = (Single (Size 1) 'y') +++ (Single (Size 1) 'e') +++ (Single (Size 1) 'a') +++ (Single (Size 1) 'h')

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i (Single m s) | i == getSize (size m) = Just s
indexJ i (Append m j1 j2) | i <= getSize (size m) =
	let
		m1 = getSize $ size $ tag j1
	in indexJ i j1 <|> indexJ (i - m1) j2
indexJ _ _ = Nothing

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ i j | i < 1 = j
dropJ i (Append _ j1 j2) = dropJ i j1 +++ dropJ (i - (getSize $ size $ tag j1)) j2
dropJ _ _ = Empty

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ i _ | i < 1 = Empty
takeJ i (Append _ j1 j2) = takeJ i j1 +++ takeJ (i - (getSize $ size $ tag j1)) j2
takeJ i j = j

{----------------------------------------------------------------------------------------------------{
                                                                      | Exercise 3
}----------------------------------------------------------------------------------------------------}

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

{----------------------------------------------------------------------------------------------------{
                                                                      | Exercise 4
}----------------------------------------------------------------------------------------------------}

instance Buffer (JoinList (Score, Size) String) where
	toString Empty = ""
	toString (Single _ s) = s
	toString (Append _ j1 j2) = toString j1 ++ toString j2

	fromString = foldl (\ x s -> x +++ Single (scoreString s, Size 1) s) Empty . lines

	line n b
		| n < 0 = Nothing
		| otherwise = indexJ (n + 1) b

	replaceLine n l b
		| n > numLines b = b
		| otherwise = takeJ (n - 1) b +++ fromString l +++ dropJ n b

	numLines = getSize . snd . tag

	value = getScore . fst . tag

testBuffer :: (JoinList (Score, Size) String)
testBuffer = foldl (+++) Empty $ map fromString
	[ "This buffer is for notes you don't want to save, and for"
	, "evaluation of steam valve coefficients."
	, "To load a different file, type the character L followed"
	, "by the name of the file."
	]

main :: IO ()
main = runEditor editor testBuffer
