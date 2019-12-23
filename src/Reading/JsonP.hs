module Reading.JsonP
  ( jsonValue
  ) where

import Control.Applicative
import Data.Char
import Reading.JsonHelper
import Reading.Parser

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ stringP "null"

jsonBool :: Parser JsonValue
jsonBool = jsonTrue <|> jsonFalse
  where
    jsonTrue = JsonBool True <$ stringP "true"
    jsonFalse = JsonBool False <$ stringP "false"

jsonNum :: Parser JsonValue
jsonNum = f <$> notNull (spanP isDigit)
  where
    f ds = JsonNum $ read ds

stringLiteral :: Parser String
stringLiteral = charP '"' *> spanP (/= '"') <* charP '"'

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

jsonArray :: Parser JsonValue
jsonArray =
  JsonArray <$> (charP '[' *> whitespace *> elements <* whitespace <* charP ']')
  where
    elements = sepBy (whitespace *> charP ',' <* whitespace) jsonValue

jsonObject :: Parser JsonValue
jsonObject =
  JsonObject <$>
  (charP '{' *> whitespace *> sepBy (whitespace *> charP ',' <* whitespace) pair <*
   whitespace <*
   charP '}')
  where
    pair =
      (\key _ value -> (key, value)) <$> stringLiteral <*>
      (whitespace *> charP ':' <* whitespace) <*>
      jsonValue

jsonValue :: Parser JsonValue
jsonValue =
  jsonNull <|> jsonBool <|> jsonNum <|> jsonString <|> jsonArray <|> jsonObject
