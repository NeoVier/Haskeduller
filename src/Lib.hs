module Lib
  ( addTask
  , readTasks
  ) where

import Data.Time

import Task

import Testing

import Data.Time.Calendar

scheduleFilePath :: FilePath
scheduleFilePath = "/home/henrique/.haskeduller.sched"

addTask :: Task -> IO ()
addTask task = appendFile scheduleFilePath (taskToString task 0 ++ "\n\n")

readTasks :: IO String
readTasks = readFile scheduleFilePath

parseTasks :: [String] -> Task
parseTasks [firstLine, description] = undefined
