{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char
import Data.Tuple (swap)

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
	f [] = Nothing    -- fail on the empty input
	f (x:xs)          -- check if x satisfies the predicate
						-- if so, return x along with the remainder
						-- of the input (that is, xs)
		| p x       = Just (x, xs)
		| otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
	f xs
	  | null ns   = Nothing
	  | otherwise = Just (read ns, rest)
	  where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

{----------------------------------------------------------------------------------------------------{
                                                                      | Exercise 1
}----------------------------------------------------------------------------------------------------}

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, b) = (f a, b)

{-
class Functor f where
	fmap :: (a -> b) -> f a -> f b
-}
instance Functor Parser where
	-- f :: a -> b
	-- a :: String -> Maybe (a, String)
	fmap f (Parser a) = Parser (fmap (first f) . a)
--	fmap f (Parser a) = Parser (\x -> first f <$> a x)

{----------------------------------------------------------------------------------------------------{
                                                                      | Exercise 2
}----------------------------------------------------------------------------------------------------}

{-
class Functor f => Applicative f where
	pure  :: a -> f a
	(<*>) :: f (a -> b) -> f a -> f b
-}

instance Applicative Parser where
	-- a :: String -> Maybe (a, String)
	pure a = Parser (\x -> Just (a, x))

	-- f :: String -> Maybe (a -> b, String)
	-- g :: String -> Maybe (a, String)
	(Parser f) <*> (Parser g) = Parser f'
		where
--			f' :: String -> Maybe (a -> b, String)
			f' x = maybe Nothing g' $ f x
			-- ^ (f x) :: Maybe (a, String)

--			g' :: (a -> b, String) -> Maybe (b, String)
			g' (f'', rest) = first f'' <$> g rest
			-- ^ (g rest) :: Maybe (a, String)
{-
The first parser has to be evaluated first.  This will give us the
partially applied result of `Maybe (a -> b, String)`.  Then we can
fmap in the result from the second parser.
-}

{----------------------------------------------------------------------------------------------------{
                                                                      | Exercise 3
}----------------------------------------------------------------------------------------------------}

abParser :: Parser (Char, Char)
abParser = ( , ) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = const () <$> abParser

intPair :: Parser ([Integer])
intPair = (\ x y -> x : y : []) <$> posInt <* char ' ' <*> posInt

{----------------------------------------------------------------------------------------------------{
                                                                      | Exercise 4
}----------------------------------------------------------------------------------------------------}

{-
class Applicative f => Alternative f where
	empty :: f a
	(<|>) :: f a -> f a -> f a
-}

instance Alternative Parser where
	empty = Parser (const Nothing)

	Parser f <|> Parser g = Parser (\x -> f x <|> g x)

intOrUppercase :: Parser ()
intOrUppercase = (const () <$> posInt) <|> (const () <$> (satisfy isUpper))
