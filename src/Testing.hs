module Testing
  ( exampleDay
  , superTask
  , complexTask
  , simpleTask
  ) where

import Data.Time
import Data.Time.Calendar
import Task

exampleDay :: Day
exampleDay = fromGregorian 2019 10 29

superTask :: Task
superTask =
  Complex
    "1"
    Todo
    "SuperTask"
    (Just exampleDay)
    "SuperDesc"
    [complexTaskWithId "1.1", simpleTaskWithId "1.2"]

complexTask :: Task
complexTask =
  Complex "2" Done "ComplexTask" Nothing "ComplexDesc" [simpleTaskWithId "2.1"]

simpleTask :: Task
simpleTask = Simple "3" None "SimpleTask" (Just exampleDay) "SimpleDesc"

complexTaskWithId :: Id -> Task
complexTaskWithId id =
  Complex
    id
    Done
    "ComplexTask"
    Nothing
    "ComplexDesc"
    [simpleTaskWithId (id ++ ".1")]

simpleTaskWithId :: Id -> Task
simpleTaskWithId id = Simple id None "SimpleTask" (Just exampleDay) "SimpleDesc"
