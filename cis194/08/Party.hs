{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Data.Monoid
import Data.Tree
import Employee

{----------------------------------------------------------------------------------------------------{
                                                                      | Exercise 1
}----------------------------------------------------------------------------------------------------}

glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp _ f1) (GL xs f2) = GL (e : xs) $ f1 + f2

instance Monoid GuestList where
	mempty = GL [] 0
	mappend gl1@(GL l1 f1) gl2@(GL l2 f2) = GL (l1 ++ l2) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ f1) gl2@(GL _ f2)
	| f1 > f2 = gl1
	| otherwise = gl2

{----------------------------------------------------------------------------------------------------{
                                                                      | Exercise 2
}----------------------------------------------------------------------------------------------------}

--treeFold :: ??? -> Tree a -> b

-- TODO

{----------------------------------------------------------------------------------------------------{
                                                                      | Exercise 3
}----------------------------------------------------------------------------------------------------}

--nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
--nextLevel e xs

-- TODO