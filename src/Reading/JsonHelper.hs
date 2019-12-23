module Reading.JsonHelper
  ( JsonValue(..)
  , jsonToString
  , mapJsonArray
  ) where

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNum Int
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Eq, Show)

jsonToString :: Int -> JsonValue -> String
jsonToString indent JsonNull = "null"
jsonToString indent (JsonBool a)
  | a = "true"
  | otherwise = "false"
jsonToString indent (JsonNum x) = show x
jsonToString indent (JsonString s) = show s
jsonToString indent (JsonArray []) = "[]"
jsonToString indent (JsonArray xs) =
  "[" ++
  init (concatMap (showElement (indent + 1)) xs) ++
  "\n" ++ indentation indent ++ "]"
  where
    showElement indent x =
      "\n" ++ indentation indent ++ jsonToString (indent + 1) x ++ ","
jsonToString indent (JsonObject []) = "{}"
jsonToString indent (JsonObject xs) =
  "{" ++ init (concatMap objToString xs) ++ "\n" ++ indentation indent ++ "}"
  where
    objToString (x, y) =
      "\n" ++
      indentation indent ++ show x ++ ": " ++ jsonToString (indent + 1) y ++ ","

indentation :: Int -> String
indentation x = concat $ replicate x "\t"

mapJsonArray :: (JsonValue -> a) -> JsonValue -> [a]
mapJsonArray f (JsonArray arr) = map f arr
mapJsonArray _ _ = error "Reading.JsonHelper.mapJsonArray: Not a JsonArray"
