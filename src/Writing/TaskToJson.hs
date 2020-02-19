module Writing.TaskToJson
  ( taskToJson
  ) where

import Reading.JsonHelper
import Task

taskToJson :: Task -> JsonValue
taskToJson (Simple id state name Nothing desc) =
  JsonObject
    [ ("identifier", JsonString id)
    , ("state", JsonString (show state))
    , ("name", JsonString name)
    , ("date", JsonString "")
    , ("description", JsonString desc)
    ]
taskToJson (Simple id state name (Just date) desc) =
  JsonObject
    [ ("identifier", JsonString id)
    , ("state", JsonString (show state))
    , ("name", JsonString name)
    , ("date", JsonString (dayToString date))
    , ("description", JsonString desc)
    ]
taskToJson (Complex id state name Nothing desc children) =
  JsonObject
    [ ("identifier", JsonString id)
    , ("state", JsonString (show state))
    , ("name", JsonString name)
    , ("date", JsonString "")
    , ("description", JsonString desc)
    , ("children", JsonArray (map taskToJson children))
    ]
taskToJson (Complex id state name (Just date) desc children) =
  JsonObject
    [ ("identifier", JsonString id)
    , ("state", JsonString (show state))
    , ("name", JsonString name)
    , ("date", JsonString (dayToString date))
    , ("description", JsonString desc)
    , ("children", JsonArray (map taskToJson children))
    ]
