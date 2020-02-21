module CommandUtils
  ( filterTasks
  , constructSimpleTask
  , constructTaskList
  , removeTaskById
  , updateTaskList
  ) where

import CommandOptions (AddFields(..), ListOptions(..), UpdateFields(..))
import Data.List.Split (splitWhen)
import Data.Maybe (isJust, isNothing)
import Data.Time
import Data.Time.Calendar
import Task

-- List command
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
constructSimpleTask :: AddFields -> Id -> Task
constructSimpleTask (AddFields name todo day description _) newId =
  Simple newId (boolToState todo) name (Just day) description

constructTaskList :: AddFields -> [Task] -> [Task]
constructTaskList fields others
  | apid fields == "" = insertSorted others newTask
  | otherwise = newTaskList
  where
    newId
      | apid fields == "" = show (length others)
      | otherwise = apid fields ++ "." ++ show (length (children oldParentTask))
    newTask = constructSimpleTask fields newId
    simpleOldParentTask = findTaskById (splitWhen (== '.') (apid fields)) others
    oldParentTask = toComplex simpleOldParentTask
    newTaskList = addChild newTask others

findTask :: Task -> [Task] -> Task
findTask t = findTaskById (splitWhen (== '.') (identifier t))

findTaskById :: [Id] -> [Task] -> Task
findTaskById [] _ = error "src.Lib.findTaskById: Id not found"
findTaskById [x] others
  | null filteredList = error "src.Lib.findTaskById: Id not found" -- = head $ filter ((== x) . lastDigit . identifier) others
  | otherwise = head filteredList
  where
    lastDigit id = last $ splitWhen (== '.') id
    filteredList = filter ((== x) . lastDigit . identifier) others
findTaskById (x:xs) others =
  findTaskById xs (children $ findTaskById [x] others)

addChild :: Task -> [Task] -> [Task]
addChild child = addChildR child (splitWhen (== '.') (identifier child))

addChildR :: Task -> [Id] -> [Task] -> [Task]
addChildR _ [] _ = error "src.Lib.addChild: Invalid Id"
addChildR child [x] taskList = insertSorted taskList child
addChildR child (x:xs) taskList =
  replace originalParentTask newParentTask taskList
  where
    originalParentTask = toComplex $ findTaskById [x] taskList
    (Complex pid pstate pname pdate pdesc pchildren) = originalParentTask
    newParentTask =
      Complex pid pstate pname pdate pdesc (addChildR child xs pchildren)

removeTask :: Task -> [Task] -> [Task]
removeTask t = removeTaskR (splitWhen (== '.') (identifier t))

removeTaskR :: [Id] -> [Task] -> [Task]
removeTaskR [x] others = filter ((/= x) . lastDigit) others
  where
    lastDigit t = last $ splitWhen (== '.') (identifier t)
removeTaskR (x:xs) others = replace oldParentTask newParentTask others
  where
    oldParentTask = findTaskById [x] others
    (Complex pid pstate pname pdate pdesc pchildren) = oldParentTask
    newParentTask =
      Complex pid pstate pname pdate pdesc (removeTaskR xs pchildren)

replace :: Task -> Task -> [Task] -> [Task]
replace old new others = insertSorted (removeTaskR [lastDigitOld] others) new
  where
    lastDigitOld = last $ splitWhen (== '.') (identifier old)

-- Assumes it's already on the right level
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

-- Remove command
removeTaskById :: Id -> [Task] -> [Task]
removeTaskById id = removeTaskR (splitWhen (== '.') id)

-- Update command
updateTaskList :: UpdateFields -> [Task] -> [Task]
updateTaskList updateFields tasks =
  addChild newTask (removeTask desiredTask tasks)
  where
    idList = splitWhen (== '.') (uid updateFields)
    desiredTask = findTaskById idList tasks
    newTask = updateTask updateFields desiredTask

updateTask :: UpdateFields -> Task -> Task
updateTask (UpdateFields id upName upDate upDesc upCycle) t
  | isComplex t =
    Complex id updatedState updatedName updatedDate updatedDesc (children t)
  | otherwise = Simple id updatedState updatedName updatedDate updatedDesc
  where
    updatedName = updatedField upName "" (name t)
    updatedDate =
      updatedField (Just upDate) (Just $ fromGregorian 1 1 1) (date t)
    updatedDesc = updatedField upDesc "" (description t)
    updatedState
      | upCycle = updateState (state t)
      | otherwise = state t

updatedField :: Eq a => a -> a -> a -> a
updatedField newValue nullValue oldValue
  | newValue == nullValue = oldValue
  | otherwise = newValue

updateState :: State -> State
updateState Todo = Done
updateState Done = None
updateState None = Todo
