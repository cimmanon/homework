{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative

import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = ((:) <$> p <*> zeroOrMore p) <|> (pure [])

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = (:)
	<$> satisfy isAlpha
	<*> zeroOrMore (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

----------------------------------------------------------------------

atom :: Parser SExpr
atom = A <$> ((N <$> posInt) <|> (I <$> ident))

-- this must have 2 or more SExpr to qualify as a Comb
comb :: Parser SExpr
comb = Comb <$> ((:) <$> expr <*> oneOrMore expr)
	where
		expr = spaces *> (atom <|> precidence) <* spaces

sexpr :: Parser SExpr
sexpr = comb <|> atom

precidence :: Parser SExpr
precidence = char '(' *> spaces *> sexpr <* spaces <* char ')'

parseSExpr :: Parser SExpr
parseSExpr = sexpr <|> precidence
