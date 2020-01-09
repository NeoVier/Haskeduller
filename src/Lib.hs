module Lib
  ( execList
  ) where

import Data.Maybe (isNothing)
import Data.Time
import Data.Time.Calendar
import Reading.JsonHelper
import Reading.JsonP
import Reading.Parser
import Reading.TaskFromJson
import Task
import Testing

scheduleFilePath :: FilePath
scheduleFilePath = "/home/henrique/.haskeduller.sched"

execList :: FilePath -> String -> IO ()
execList file period = do
  jsonTasks <- parseFile file jsonValue
  case jsonTasks of
    Just contents -> mapM_ print (filterTasks period tasks)
      where tasks = mapJsonArray taskFromJson contents
    Nothing -> putStrLn $ parseError file

filterTasks :: String -> [Task] -> [Task]
filterTasks "without" xs = filter (isNothing . date) xs
filterTasks "all" xs = xs
