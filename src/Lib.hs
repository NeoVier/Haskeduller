module Lib
  ( execList
  , execAdd
  ) where

import CommandOptions (AddFields(..), ListOptions(..))
import CommandUtils
import Data.Time
import Reading.JsonHelper
import Reading.JsonP
import Reading.Parser
import Reading.TaskFromJson
import System.Directory (getHomeDirectory, renameFile)
import Writing.Write

scheduleFilePath :: FilePath
scheduleFilePath = "/home/henrique/.haskeduller.sched"

-- List command
execList :: FilePath -> ListOptions -> IO ()
execList file period = do
  jsonTasks <- parseFile file jsonValue
  case jsonTasks of
    Just contents -> do
      today <- utctDay <$> getCurrentTime
      mapM_ print (filterTasks period today tasks)
      where tasks = mapJsonArray taskFromJson contents
    Nothing -> putStrLn $ parseError file

-- Add command
execAdd :: FilePath -> AddFields -> IO ()
execAdd file addFields = do
  jsonTasks <- parseFile file jsonValue
  case jsonTasks of
    Just contents -> do
      homeDirectory <- getHomeDirectory
      let tempFile = homeDirectory ++ "/haskedullertmp.json"
      let tasks = mapJsonArray taskFromJson contents
      let newTaskList = constructTaskList addFields tasks
      writeTasks newTaskList tempFile
      renameFile tempFile file
    Nothing -> do
      let newTask = constructSimpleTask addFields "0"
      writeTasks [newTask] file
