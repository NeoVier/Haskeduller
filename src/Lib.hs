module Lib
  ( execList
  , execAdd
  , execRemove
  , execUpdate
  ) where

import CommandOptions (AddFields(..), ListOptions(..), UpdateFields(..))
import CommandUtils
import Data.Time
import Reading.JsonHelper
import Reading.JsonP
import Reading.Parser
import Reading.TaskFromJson
import System.Directory (doesFileExist, getHomeDirectory, renameFile)
import Task
import Writing.Write

getTasks :: JsonValue -> [Task]
getTasks = mapJsonArray taskFromJson

-- List command
execList :: FilePath -> ListOptions -> IO ()
execList file period = do
  jsonTasks <- parseFile file jsonValue
  case jsonTasks of
    Just contents -> do
      today <- utctDay <$> getCurrentTime
      let tasks = getTasks contents
      mapM_ print (filterTasks period today tasks)
    Nothing -> putStrLn $ parseError file

-- Add command
execAdd :: FilePath -> AddFields -> IO ()
execAdd file addFields = do
  fileExists <- doesFileExist file
  if fileExists
    then addExistingFile file addFields
    else addNewFile file addFields

addNewFile :: FilePath -> AddFields -> IO ()
addNewFile file addFields = do
  let newTask = constructSimpleTask addFields "0"
  writeTasks [newTask] file

addExistingFile :: FilePath -> AddFields -> IO ()
addExistingFile file addFields = do
  jsonTasks <- parseFile file jsonValue
  case jsonTasks of
    Just contents -> do
      let tasks = getTasks contents
      let newTaskList = constructTaskList addFields tasks
      writeTasks newTaskList file
    Nothing -> do
      let newTask = constructSimpleTask addFields "0"
      writeTasks [newTask] file

-- Remove command
execRemove :: FilePath -> Id -> IO ()
execRemove file id = do
  jsonTasks <- parseFile file jsonValue
  case jsonTasks of
    Just contents -> do
      let tasks = getTasks contents
      let newTaskList = removeTaskById id tasks
      writeTasks newTaskList file
    Nothing -> putStrLn $ parseError file

-- Update command
execUpdate :: FilePath -> UpdateFields -> IO ()
execUpdate file updateFields = do
  jsonTasks <- parseFile file jsonValue
  case jsonTasks of
    Just contents -> do
      let tasks = getTasks contents
      let newTaskList = updateTaskList updateFields tasks
      writeTasks newTaskList file
    Nothing -> putStrLn $ parseError file
