module PrettyJSON
  (
    renderJSONValue
  ) where

import Numeric (showHex)
import Data.Char (ord)
import Data.Bits (shiftR, (.&.))

import JSONParser (JSONValue(..))
import Prettify (Doc, (<>), char, integer, fsep, hcat, punctuate, text,
                 compact, pretty)

renderJSONValue :: JSONValue -> Doc
renderJSONValue (Bool True)  = text "true"
renderJSONValue (Bool False) = text "false"
renderJSONValue Null         = text "null"
renderJSONValue (Number num) = integer num
renderJSONValue (String str) = string str
renderJSONValue (Array a)    = series '[' ']' renderJSONValue a
renderJSONValue (Object obj) = series '{' '}' field obj
  where field (key, val) = string key
                          <> text ": "
                          <> renderJSONValue val

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right

oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
              Just r -> text r
              Nothing | mustEscape c -> hexEscape c
                      | otherwise    -> char c
                      where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
  where ch a b = (a, ['\\',b])

-- Can only represent Unicode characters up to 0xffff.
smallHex :: Int -> Doc
smallHex x  = text "\\u"
           <> text (replicate (4 - length h) '0')
           <> text h
             where h = showHex x ""

hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d
            | otherwise   = astral (d - 0x10000)
            where d = ord c

-- Represent Unicode characters above 0xffff in a JSON string.
astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
  where a = (n `shiftR` 10) .&. 0x3ff
        b = n .&. 0x3ff

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item = enclose open close
                       . fsep . punctuate (char ',') . map item
