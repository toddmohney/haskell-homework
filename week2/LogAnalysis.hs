{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
  import Log

  -- Exercise 1
  parseMessage :: String -> LogMessage
  parseMessage = parseMessage' . words
    where
      parseMessage' :: [String] -> LogMessage
      parseMessage' ("I":ts:desc)             = buildLogMessage Info ts desc
      parseMessage' ("W":ts:desc)             = buildLogMessage Warning ts desc
      parseMessage' ("E":severity:ts:desc)    = buildLogMessage (Error (read severity)) ts desc
      parseMessage' str                       = Unknown (unwords str)

      buildLogMessage :: MessageType -> String -> [String] -> LogMessage
      buildLogMessage msgType ts desc = LogMessage msgType (read ts) (unwords desc)

  parse :: String -> [LogMessage]
  parse = map parseMessage . lines
  --

  -- Exercise 2
  insert :: LogMessage -> MessageTree -> MessageTree
  insert (Unknown _) tree = tree
  insert msg Leaf = Node Leaf msg Leaf
  insert msg@(LogMessage _ ts _) (Node left msg2@(LogMessage _ ts2 _) right)
    | ts < ts2  = Node (insert msg left) msg2 right
    | otherwise = Node left msg2 (insert msg right)
  insert LogMessage{} (Node _ (Unknown _) _) = undefined
  --

  -- Exercise 3
  build :: [LogMessage] -> MessageTree
  build = foldr insert Leaf
  --

  -- Exercise 4
  inOrder :: MessageTree -> [LogMessage]
  inOrder Leaf = []
  inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right
  --

  -- Exercise 5
  whatWentWrong :: [LogMessage] -> [String]
  whatWentWrong = map message . inOrder . build . filter severeError
    where
      severeError :: LogMessage -> Bool
      severeError (LogMessage (Error severity) _ _) = severity >= severityThreshold
      severeError _                                 = False

      severityThreshold :: (Num a) => a
      severityThreshold = 50

      message :: LogMessage -> String
      message (LogMessage _ _ msg) = msg
      message (Unknown msg)        = msg
  --
