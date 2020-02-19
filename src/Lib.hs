module Lib
  ( execList
  , execAdd
  ) where

import CommandOptions (AddFields(..), ListOptions(..))
import Data.Maybe (isJust, isNothing)
import Data.Time
import Data.Time.Calendar
import Reading.JsonHelper
import Reading.JsonP
import Reading.Parser
import Reading.TaskFromJson
import System.Directory (getHomeDirectory, renameFile)
import Task
import Testing
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

filterTasks :: ListOptions -> Day -> [Task] -> [Task]
filterTasks Today today xs = filter (sameDay today . date) xs
filterTasks Tomorrow today xs = filter (sameDay tomorrow . date) xs
  where
    tomorrow = addDays 1 today
filterTasks (OneDay day) _ xs = filter (sameDay day . date) xs
filterTasks ThisWeek today xs =
  filter
    (\x ->
       case date x of
         Nothing -> False
         Just d -> diffDays d today < 7 && diffDays d today >= 0)
    xs
filterTasks NextWeek today xs =
  filterTasks ThisWeek (findSunday (addDays 7 today)) xs
filterTasks (OneWeek day) _ xs = filterTasks ThisWeek (findSunday day) xs
filterTasks WithoutDate _ xs = filter (isNothing . date) xs
filterTasks WithDate _ xs = filter (isJust . date) xs
filterTasks All _ xs = xs

findSunday day
  | mod7 < 4 = addDays (-mod7 - 3) day
  | mod7 == 4 = day
  | mod7 > 4 = addDays (4 - mod7) day
  where
    num = toInteger $ fromEnum day
    mod7 = num `rem` 7

sameDay :: Day -> Maybe Day -> Bool
sameDay day Nothing = False
sameDay day (Just other) = day == other

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
      print newTaskList
    Nothing -> do
      let newTask = constructSimpleTask addFields "0"
      writeTasks [newTask] file
      print newTask

constructSimpleTask :: AddFields -> Id -> Task
constructSimpleTask (Fields name todo day description _) newId =
  Simple newId (boolToState todo) name (Just day) description

constructTaskList :: AddFields -> [Task] -> [Task]
constructTaskList fields others
  | fpid fields == "" = newTask : others
  | otherwise = addChild newTask parentTask : filter (/= parentTask) others
  where
    newId = show (1 + length others)
    newTask = constructSimpleTask fields newId
    parentTask = head $ filter ((== fpid fields) . identifier) others

addChild :: Task -> Task -> Task
addChild child (Simple id state name date desc) =
  Complex id state name date desc [child]
addChild child (Complex id state name date desc children) =
  Complex id state name date desc (child : children)
