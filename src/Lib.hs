module Lib
  ( execList
  , execAdd
  , execRemove
  ) where

import CommandOptions (AddFields(..), ListOptions(..))
import CommandUtils
import Data.Time
import Reading.JsonHelper
import Reading.JsonP
import Reading.Parser
import Reading.TaskFromJson
import System.Directory (getHomeDirectory, renameFile)
import Task
import Writing.Write

scheduleFilePath :: FilePath
scheduleFilePath = "/home/henrique/.haskeduller.sched"

getTmpFile :: IO FilePath
getTmpFile = do
  homeDir <- getHomeDirectory
  return $ homeDir ++ "/.cache/haskedullertmp.json"

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
      tmpFile <- getTmpFile
      let tasks = mapJsonArray taskFromJson contents
      let newTaskList = constructTaskList addFields tasks
      writeTasks newTaskList tmpFile
      renameFile tmpFile file
    Nothing -> do
      let newTask = constructSimpleTask addFields "0"
      writeTasks [newTask] file

-- Remove command
execRemove :: FilePath -> Id -> IO ()
execRemove file id = do
  jsonTasks <- parseFile file jsonValue
  case jsonTasks of
    Just contents -> do
      let tasks = mapJsonArray taskFromJson contents
      let newTaskList = removeTaskById id tasks
      tmpFile <- getTmpFile
      writeTasks newTaskList tmpFile
      renameFile tmpFile file
    Nothing -> putStrLn $ parseError file
