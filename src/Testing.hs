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
    Todo
    "SuperTask"
    (Just exampleDay)
    "SuperDesc"
    [complexTask, simpleTask]

complexTask :: Task
complexTask = Complex Done "ComplexTask" Nothing "ComplexDesc" [simpleTask]

simpleTask :: Task
simpleTask = Simple None "SimpleTask" (Just exampleDay) "SimpleDesc"
