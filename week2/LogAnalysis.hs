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


{- sortMessages :: [LogMessage] -> MessageTree -}
