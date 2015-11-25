{-# LANGUAGE FlexibleInstances #-}

module Calc where

import ExprT
import Parser
--import StackVM

import qualified Data.Map as M
import Control.Applicative

{----------------------------------------------------------------------------------------------------{
                                                                      | Exercise 1
}----------------------------------------------------------------------------------------------------}

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

{----------------------------------------------------------------------------------------------------{
                                                                      | Exercise 2
}----------------------------------------------------------------------------------------------------}

evalStr :: String -> Maybe Integer
evalStr = (fmap eval) . parseExp Lit Add Mul

{----------------------------------------------------------------------------------------------------{
                                                                      | Exercise 3
}----------------------------------------------------------------------------------------------------}

class Expr a where
	lit :: Integer -> a
	mul :: a -> a -> a
	add :: a -> a -> a

instance Expr ExprT where
	lit i = Lit i
	mul e1 e2 = Mul e1 e2
	add e1 e2 = Add e1 e2

{----------------------------------------------------------------------------------------------------{
                                                                      | Exercise 4
}----------------------------------------------------------------------------------------------------}

instance Expr Integer where
	lit = id
	mul = (*)
	add = (+)

instance Expr Bool where
	lit i
		| i > 0 = True
		| otherwise = False
	mul = (&&)
	add = (||)

instance Expr MinMax where
	lit = MinMax
	mul (MinMax i1) (MinMax i2) = lit $ min i1 i2
	add (MinMax i1) (MinMax i2) = lit $ max i1 i2

instance Expr Mod7 where
	lit = Mod7 . (flip mod) 7
	mul (Mod7 i1) (Mod7 i2) = lit $ i1 * i2
	add (Mod7 i1) (Mod7 i2) = lit $ i1 + i2

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

---------------------------------------------------------------------- | Tests

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

{----------------------------------------------------------------------------------------------------{
                                                                      | Exercise 5
}----------------------------------------------------------------------------------------------------}

-- TODO

{----------------------------------------------------------------------------------------------------{
                                                                      | Exercise 6
}----------------------------------------------------------------------------------------------------}

data VarExprT = VLit Integer
           | VAdd VarExprT VarExprT
           | VMul VarExprT VarExprT
           | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
	lit i = VLit i
	mul e1 e2 = VMul e1 e2
	add e1 e2 = VAdd e1 e2

class HasVars a where
	var :: String -> a

instance HasVars VarExprT where
	var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
	var m = M.lookup m

instance Expr (M.Map String Integer -> Maybe Integer) where
	lit i _ = Just i
	mul e1 e2 m = (*) <$> e1 m <*> e2 m
	add e1 e2 m = (+) <$> e1 m <*> e2 m

withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
