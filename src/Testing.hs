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
    1
    Todo
    "SuperTask"
    (Just exampleDay)
    "SuperDesc"
    [complexTask, simpleTask]

complexTask :: Task
complexTask = Complex 2 Done "ComplexTask" Nothing "ComplexDesc" [simpleTask]

simpleTask :: Task
simpleTask = Simple 3 None "SimpleTask" (Just exampleDay) "SimpleDesc"
