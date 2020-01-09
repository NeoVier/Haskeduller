module Lib
  ( listAll
  ) where

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

listAll :: FilePath -> IO ()
listAll file = do
  tasks <- parseFile file jsonValue
  case tasks of
    Just contents -> mapM_ print (mapJsonArray taskFromJson contents)
    Nothing -> putStrLn $ parseError file
