module Lib
  ( executeJSon
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

executeJSon :: FilePath -> IO ()
executeJSon file = do
  result <- parseFile file jsonValue
  case result of
    Nothing -> putStrLn $ parseError file
    Just contents -> print (mapJsonArray taskFromJson contents)

parseError :: FilePath -> String
parseError file = "Could not parse file: " ++ file
