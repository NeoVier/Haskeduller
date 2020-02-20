module Lib
  ( execList
  , execAdd
  , addChild
  , insertSorted
  , removeTask
  ) where

import CommandOptions (AddFields(..), ListOptions(..))
import Data.List.Split (splitWhen)
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
    Nothing -> do
      let newTask = constructSimpleTask addFields "0"
      writeTasks [newTask] file

constructSimpleTask :: AddFields -> Id -> Task
constructSimpleTask (Fields name todo day description _) newId =
  Simple newId (boolToState todo) name (Just day) description

constructTaskList :: AddFields -> [Task] -> [Task]
constructTaskList fields others
  | fpid fields == "" = insertSorted others newTask
  | otherwise = newTaskList -- filter (/= oldParentTask) others ++ [parentTask]
  where
    newId
      | fpid fields == "" = show (length others)
      | otherwise = fpid fields ++ "." ++ show (length (children oldParentTask))
    newTask = constructSimpleTask fields newId
    simpleOldParentTask = findTask (splitWhen (== '.') (fpid fields)) others
    oldParentTask = toComplex simpleOldParentTask
    newTaskList = addChild newTask others

findTask :: [Id] -> [Task] -> Task
findTask [] _ = error "src.Lib.findTask: Id not found"
findTask [x] others = head $ filter ((== x) . lastDigit . identifier) others
  where
    lastDigit id = last $ splitWhen (== '.') id
findTask (x:xs) others = findTask xs (children $ findTask [x] others)

addChild :: Task -> [Task] -> [Task]
addChild child = addChildR child (splitWhen (== '.') (identifier child))

addChildR :: Task -> [Id] -> [Task] -> [Task]
addChildR _ [] _ = error "src.Lib.addChild: Invalid Id"
addChildR child [x] taskList = insertSorted taskList child -- Duplicating parent?
addChildR child (x:xs) taskList =
  replace originalParentTask newParentTask taskList
  where
    originalParentTask = toComplex $ findTask [x] taskList
    (Complex pid pstate pname pdate pdesc pchildren) = originalParentTask
    newParentTask =
      Complex pid pstate pname pdate pdesc (addChildR child xs pchildren)

removeTask :: Task -> [Task] -> [Task]
removeTask t = removeTaskR (splitWhen (== '.') (identifier t)) -- filter (/= t)

removeTaskR :: [Id] -> [Task] -> [Task]
removeTaskR [x] others = filter ((/= x) . lastDigit) others
  where
    lastDigit t = last $ splitWhen (== '.') (identifier t)
removeTaskR (x:xs) others = replace oldParentTask newParentTask others
  where
    oldParentTask = findTask [x] others
    (Complex pid pstate pname pdate pdesc pchildren) = oldParentTask
    newParentTask =
      Complex pid pstate pname pdate pdesc (removeTaskR xs pchildren)

replace :: Task -> Task -> [Task] -> [Task]
replace old new others = insertSorted (removeTask old others) new

insertSorted :: [Task] -> Task -> [Task]
insertSorted [] x = [x]
insertSorted (y:ys) x
  | (read (listId x !! lenIdY) :: Integer) <= (read (lastDigit y) :: Integer) =
    x : y : ys
  | otherwise = y : insertSorted ys x
  where
    listId t = splitWhen (== '.') (identifier t)
    lastDigit t = last $ listId t
    lenIdY = length (listId y) - 1
