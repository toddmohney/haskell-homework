{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage x = let wordlist = words x in
  case wordlist of
    ("I":timestamp:message)          -> LogMessage Info (read timestamp) (unwords message)
    ("W":timestamp:message)          -> LogMessage Warning (read timestamp) (unwords message)
    ("E":severity:timestamp:message) -> LogMessage (Error (read severity)) (read timestamp) (unwords message)
    _                                -> Unknown (unwords wordlist)


parse :: String -> [LogMessage]
parse = map parseMessage . lines


insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) messageTree = messageTree -- ignore unknown messages
insert logMessage Leaf         = (Node Leaf logMessage Leaf) -- base case, insert new message node
insert logMessage (Node leftMsg msg rightMsg)
  | (timestamp logMessage) > (timestamp msg) = Node leftMsg msg (insert logMessage rightMsg)
  | otherwise                                = Node (insert logMessage leftMsg) msg rightMsg
  where
    timestamp :: LogMessage -> TimeStamp
    timestamp (LogMessage _ ts _) = ts
    timestamp (Unknown _)         = undefined


build :: [LogMessage] -> MessageTree
build = foldr insert Leaf





