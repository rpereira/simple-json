{-
    Abstract syntax & parser for JSON values (http://www.json.org)
    Pedro Vasconcelos, 2016
    Modified by Rui Afonso Pereira, 2017
-}

module JSONParser
  (
    JSONValue(..)
  ) where

import Text.Parsec
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as P

-- a string parser with no user-state
type Parser = Parsec String ()

{- abstract syntax for JSON values
   (http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-404.pdf)
-}
data JSONValue = Number Integer
               -- ^ NB: floating-point values ommitted for brevity
               | String String
               | Bool Bool
               | Null
               | Array [JSONValue]
               | Object [(String, JSONValue)]
               deriving (Eq, Ord, Show)

--- tokenizer
lexer         = P.makeTokenParser emptyDef
whiteSpace    = P.whiteSpace lexer
symbol        = P.symbol lexer
reserved      = P.reserved lexer
integer       = P.integer lexer
stringLiteral = P.stringLiteral lexer

--- syntax rules
jsonValue :: Parser JSONValue
jsonValue = do { n <- integer; return (Number n) }
            <|> do { b <- boolean; return (Bool b) }
            <|> do { s <- stringLiteral; return (String s) }
            <|> do { reserved "null"; return Null }
            <|> do { symbol "[";
                     vs <- jsonValue `sepBy` comma;
                     symbol "]";
                     return (Array vs)
                   }
            <|> do { symbol "{";
                     pairs <- keyValuePair `sepBy` comma;
                     symbol "}";
                     return (Object pairs)
                   }

keyValuePair :: Parser (String, JSONValue)
keyValuePair = do { n <- stringLiteral;
                    colon;
                    v <- jsonValue;
                    return (n, v)
                  }

comma = symbol ","
colon = symbol ":"

boolean :: Parser Bool
boolean = do { reserved "true"; return True } <|>
          do { reserved "false"; return False }

--- File input
readJson :: FilePath -> IO JSONValue
readJson filepath
  = do { contents <- readFile filepath
       ; case parse topValue filepath contents of
         Left err -> error (show err)
         Right val -> return val
       }

topValue = do whiteSpace; v <- jsonValue; eof; return v
