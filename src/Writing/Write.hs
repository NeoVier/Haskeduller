module Writing.Write
  ( writeTasks
  ) where

import Reading.JsonHelper
import System.Directory (getHomeDirectory, renameFile)
import Task
import Writing.TaskToJson

getTmpFile :: IO FilePath
getTmpFile = do
  homeDir <- getHomeDirectory
  return $ homeDir ++ "/.cache/haskedullertmp.json"

writeTasks :: [Task] -> FilePath -> IO ()
writeTasks tasks file = do
  let jsonTasks = JsonArray (map taskToJson tasks)
  let txtTasks = jsonToString 0 jsonTasks
  tmpFile <- getTmpFile
  writeFile tmpFile txtTasks
  renameFile tmpFile file
