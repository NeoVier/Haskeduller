module Writing.Write
  ( writeTasks
  ) where

import Reading.JsonHelper
import Task
import Writing.TaskToJson

writeTasks :: [Task] -> FilePath -> IO ()
writeTasks tasks file = do
  let jsonTasks = JsonArray (map taskToJson tasks)
  let txtTasks = jsonToString 0 jsonTasks
  writeFile file txtTasks
