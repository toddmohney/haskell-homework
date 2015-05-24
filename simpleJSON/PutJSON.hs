module PutJSON 
  (
    renderJSONValue
  , putJSONValue
  ) where

  import Data.List (intercalate)
  import SimpleJSON

  renderJSONValue :: JSONValue -> String
  renderJSONValue (JSONString str) = show str
  renderJSONValue (JSONNumber num) = show num
  renderJSONValue (JSONBool True)  = "true"
  renderJSONValue (JSONBool False) = "false"
  renderJSONValue JSONNull         = "null"

  renderJSONValue (JSONObject obj) = "{" ++ (renderObjs obj) ++ "}"
    where renderObjs [] = ""
          renderObjs xs = intercalate "," (map renderPair xs)
          renderPair (k, v) = show k ++ ": " ++ renderJSONValue v

  renderJSONValue (JSONArray arr) = "[" ++ (values arr) ++ "]"
      where values [] = ""
            values items = intercalate "," (map renderJSONValue items)

  putJSONValue :: JSONValue -> IO ()
  putJSONValue json = putStrLn (renderJSONValue json)
