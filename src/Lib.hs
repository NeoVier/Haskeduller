module Lib
  --( addTask
  --, readTasks
  (
  ) where

import Data.Time
import Data.Time.Calendar
import Task
import Testing

scheduleFilePath :: FilePath
scheduleFilePath = "/home/henrique/.haskeduller.sched"
--addTask :: Task -> IO ()
--addTask task = appendFile scheduleFilePath (taskToString task 0 ++ "\n\n")
--readTasks :: IO String
--readTasks = readFile scheduleFilePath
--parseTasks :: [String] -> Task
--parseTasks [firstLine, description] = undefined
