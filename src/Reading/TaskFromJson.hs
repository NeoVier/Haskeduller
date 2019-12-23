module Reading.TaskFromJson
  ( taskFromJson
  ) where

import Data.Time
import Reading.JsonHelper
import Reading.JsonP
import Task

taskFromJson :: JsonValue -> Task
taskFromJson (JsonObject [("identifier", id), ("state", state), ("name", name), ("date", date), ("description", description)]) =
  Simple
    (idFromJson id)
    (stateFromJson state)
    (nameFromJson name)
    (dateFromJson date)
    (descriptionFromJson description)
taskFromJson (JsonObject [("identifier", id), ("state", state), ("name", name), ("date", date), ("description", description), ("children", children)]) =
  Complex
    (idFromJson id)
    (stateFromJson state)
    (nameFromJson name)
    (dateFromJson date)
    (descriptionFromJson description)
    (mapJsonArray taskFromJson children)
taskFromJson _ = error "Reading.TaskFromJson.taskFromJson: Invalid task"

idFromJson :: JsonValue -> Id
idFromJson (JsonString id) = id

stateFromJson :: JsonValue -> State
stateFromJson (JsonString "TODO") = Todo
stateFromJson (JsonString "DONE") = Done
stateFromJson (JsonString "NONE") = None
stateFromJson _ = error "Reading.TaskFromJson.stateFromJson: Invalid state"

nameFromJson :: JsonValue -> Name
nameFromJson (JsonString name) = name

dateFromJson :: JsonValue -> Maybe Day
dateFromJson (JsonString date) =
  parseTimeM True defaultTimeLocale "%d/%m/%Y" date

descriptionFromJson :: JsonValue -> Description
descriptionFromJson (JsonString desc) = desc
