module Task
  ( Task(Simple, Complex)
  , State(Todo, Done, None)
  , taskToString
  , dayToString
  ) where

import Data.Time
import Data.Time.Calendar

type Name = String

type Description = String

data State
  = Todo
  | Done
  | None

instance Show State where
  show Todo = "TODO: "
  show Done = "DONE: "
  show None = ""

data Task
  = Simple
      { state :: State
      , name :: Name
      , date :: Maybe Day
      , description :: Description
      }
  | Complex
      { state :: State
      , name :: Name
      , date :: Maybe Day
      , description :: Description
      , children :: [Task]
      }

instance Show Task where
  show t = taskToString t 0

taskToString :: Task -> Integer -> String
taskToString (Complex state name day desc subtasks) indentation =
  taskToString (Simple state name day desc) indentation ++ childrenTasks
  where
    childrenTasks =
      concat
        ["\n\n" ++ taskToString subtask (indentation + 1) | subtask <- subtasks]
taskToString (Simple state name day desc) indentation =
  case day of
    Nothing -> indent ++ show state ++ name ++ "\n\t" ++ indent ++ desc
    Just day ->
      indent ++
      show state ++ name ++ ": " ++ dayToString day ++ "\n\t" ++ indent ++ desc
  where
    indent = concat ["\t" | _ <- [1 .. indentation]]

dayToString :: Day -> String
dayToString day = show d ++ "-" ++ show m ++ "-" ++ show y
  where
    (y, m, d) = toGregorian day
