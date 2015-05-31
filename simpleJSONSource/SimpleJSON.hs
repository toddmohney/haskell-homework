module SimpleJSON 
  (
    JSONValue(..)
  , getString
  , getInt
  , getDouble
  , getBool
  , getObject
  , getArray
  , isNull
  ) where

  data JSONValue = JSONString String
                 | JSONNumber Double
                 | JSONBool Bool 
                 | JSONNull 
                 | JSONObject [(String, JSONValue)]
                 | JSONArray [JSONValue]
                 deriving (Eq, Ord, Show)

  getString :: JSONValue -> Maybe String
  getString (JSONString s) = Just s
  getString _              = Nothing

  getInt :: JSONValue -> Maybe Int
  getInt (JSONNumber i) = Just (truncate i)
  getInt _           = Nothing

  getDouble :: JSONValue -> Maybe Double
  getDouble (JSONNumber d) = Just d
  getDouble _              = Nothing

  getBool :: JSONValue -> Maybe Bool
  getBool (JSONBool b) = Just b
  getBool _            = Nothing

  getObject :: JSONValue -> Maybe [(String, JSONValue)]
  getObject (JSONObject o) = Just o
  getObject _              = Nothing

  getArray :: JSONValue -> Maybe [JSONValue]
  getArray (JSONArray a) = Just a
  getArray _             = Nothing

  isNull :: JSONValue -> Bool
  isNull v = v == JSONNull
