module Main where
  import SimpleJSON

  main :: IO ()
  main = print (JSONObject [("foo", JSONNumber 799), ("bar", JSONBool True)])
