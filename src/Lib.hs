module Lib
  ( execList
  , getToday
  ) where

import CommandOptions (ListOptions(..))
import Data.Maybe (isJust, isNothing)
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
filterTasks ThisWeek today xs = undefined
filterTasks NextWeek today xs = undefined
filterTasks (OneWeek day) today xs = undefined
filterTasks WithoutDate _ xs = filter (isNothing . date) xs
filterTasks All _ xs = xs

sameDay :: Day -> Maybe Day -> Bool
sameDay day Nothing = False
sameDay day (Just other) = day == other

stringToDay :: String -> Maybe Day
stringToDay = parseTimeM True defaultTimeLocale "%d/%m/%Y"

getToday :: IO String
getToday = do
  day <- utctDay <$> getCurrentTime
  return (dayToString day)
