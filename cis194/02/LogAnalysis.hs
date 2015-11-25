{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}

module LogAnalysis where

import Log

{----------------------------------------------------------------------------------------------------{
                                                                      | Exercise 1
}----------------------------------------------------------------------------------------------------}

parseMessage :: String -> LogMessage
parseMessage = parseMessage' . words

parseMessage' :: [String] -> LogMessage
parseMessage' ("I":t:xs) = LogMessage Info (read t) $ unwords xs
parseMessage' ("W":t:xs) = LogMessage Warning (read t) $ unwords xs
parseMessage' ("E":sev:t:xs) = LogMessage (Error $ read sev) (read t) $ unwords xs
parseMessage' xs = Unknown $ unwords xs

parse :: String -> [LogMessage]
parse = map parseMessage . lines

{----------------------------------------------------------------------------------------------------{
                                                                      | Exercise 2
}----------------------------------------------------------------------------------------------------}

insert :: LogMessage -> MessageTree -> MessageTree
insert m Leaf = Node Leaf m Leaf
insert (Unknown _) t = t
insert m1 (Node t1 m2 t2)
	| m1 > m2 = Node t1 m2 (insert m1 t2)
	| otherwise = Node (insert m1 t1) m2 t2

{----------------------------------------------------------------------------------------------------{
                                                                      | Exercise 3
}----------------------------------------------------------------------------------------------------}

build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

{----------------------------------------------------------------------------------------------------{
                                                                      | Exercise 4
}----------------------------------------------------------------------------------------------------}

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node t1 m t2) = concat [inOrder t1, [m], inOrder t2]

{----------------------------------------------------------------------------------------------------{
                                                                      | Exercise 5
}----------------------------------------------------------------------------------------------------}

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = (map getMessage) . inOrder . build . filter (> (LogMessage (Error 50) 0 ""))

getMessage :: LogMessage -> String
getMessage (Unknown x) = x
getMessage (LogMessage _ _ x) = x

-- this is probably not how the problem is expected to be solved, but it works
deriving instance Ord MessageType
deriving instance Ord LogMessage
