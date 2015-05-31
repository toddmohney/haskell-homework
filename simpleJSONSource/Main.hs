module Main where
  import Prettify
  import PrettyJSON
  import SimpleJSON

  main :: IO ()
  main = let obj1     = JSONObject [("foo", JSONNumber 799), ("bar", JSONBool True)]
             innerObj = JSONObject [("ack", JSONNumber 23982983.23298), ("blah", JSONString "this is super important text for you")]
             obj2     = JSONObject [("baz", innerObj), ("falsey", JSONBool False)]
             doc      = renderJSONValue (JSONObject [("thing1", obj1), ("thing2", obj2)]) in
    putStrLn (pretty 80 doc)
