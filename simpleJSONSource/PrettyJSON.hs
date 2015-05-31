module PrettyJSON
  (
    renderJSONValue
  , pretty
  ) where

  import Data.Bits (shiftR, (.&.))
  import Data.Char (ord)
  import Data.List (lookup)
  import Numeric (showHex)
  import Prettify (Doc, (<>), char, double, fsep, hcat, punctuate, text,
                   compact, pretty)
  import SimpleJSON

  renderJSONValue :: JSONValue -> Doc
  renderJSONValue (JSONNumber num) = double num
  renderJSONValue (JSONString str) = string str
  renderJSONValue (JSONBool True)  = text "true"
  renderJSONValue (JSONBool False) = text "false"
  renderJSONValue JSONNull         = text "null"
  renderJSONValue (JSONArray arr)  = series '[' ']' renderJSONValue arr
  renderJSONValue (JSONObject obj) = series '{' '}' field obj
      where field (name,val) = string name
                            <> text ": "
                            <> renderJSONValue val

  enclose :: Char -> Char -> Doc -> Doc
  enclose left right doc = char left <> doc <> char right

  string :: String -> Doc
  string = enclose '"' '"' . hcat . map oneChar

  oneChar :: Char -> Doc
  oneChar c = case lookup c simpleEscapes of
                Just r -> text r
                Nothing | mustEscape c -> hexEscape c
                        | otherwise    -> char c
    where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

  simpleEscapes :: [(Char, String)]
  simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
    where ch a b = (a, ['\\',b])

  hexEscape :: Char -> Doc
  hexEscape c | d < 0x10000 = smallHex d
              | otherwise   = astral (d - 0x10000)
    where d = ord c

  astral :: Int -> Doc
  astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
      where a = (n `shiftR` 10) .&. 0x3ff
            b = n .&. 0x3ff

  smallHex :: Int -> Doc
  smallHex x  = text "\\u"
             <> text (replicate (4 - length h) '0')
             <> text h
      where h = showHex x ""

  series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
  series open close item = enclose open close . fsep . punctuate (char ',') . map item

