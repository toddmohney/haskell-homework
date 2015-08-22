{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
  import Log

  -- Exercise 1
  parseMessage :: String -> LogMessage
  parseMessage = parseMessage' . words
    where 
      parseMessage' :: [String] -> LogMessage
      parseMessage' ("I":ts:xs)          = LogMessage Info (read ts) (unwords xs)
      parseMessage' ("W":ts:xs)          = LogMessage Warning (read ts) (unwords xs)
      parseMessage' ("E":severity:ts:xs) = LogMessage (Error (read severity)) (read ts) (unwords xs)
      parseMessage' str                  = Unknown (unwords str)

  parse :: String -> [LogMessage]
  parse = map parseMessage . lines
  --

  -- Exercise 2
  insert :: LogMessage -> MessageTree -> MessageTree
  insert (Unknown _) tree = tree
  insert logMessage Leaf  = Node Leaf logMessage Leaf
  insert msg@(LogMessage _ ts _) (Node l msg2@(LogMessage _ ts2 _) r)
    | ts < ts2  = Node (insert msg l) msg2 r
    | otherwise = Node l msg2 (insert msg r)
  insert LogMessage{} (Node _ (Unknown _) _) = undefined -- we must provide this pattern to avoid 
                                                         -- warnings about non-exhaustive pattern matching
  --

  -- Exercise 3
  build :: [LogMessage] -> MessageTree
  build = foldr insert Leaf
  --

  -- Exercise 4
  inOrder :: MessageTree -> [LogMessage]
  inOrder Leaf = []
  inOrder (Node l msg r) = inOrder l ++ [msg] ++ inOrder r
  --

  -- Exercise 5
  whatWentWrong :: [LogMessage] -> [String]
  whatWentWrong = map getMessage . inOrder . build . filter severeError
    where
      severeError :: LogMessage -> Bool
      severeError (LogMessage (Error severity) _ _) = severity >= severityThreshold
      severeError _                                 = False

      severityThreshold :: (Num a) => a
      severityThreshold = 50

      getMessage :: LogMessage -> String
      getMessage (LogMessage _ _ msg) = msg
      getMessage (Unknown msg)        = msg
  --

