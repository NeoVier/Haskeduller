module Task
  ( Task(Simple, Complex)
  , taskToString
  , dayToString
  ) where

import Data.Time
import Data.Time.Calendar

type Name = String

type Description = String

data Task
  = Simple
      { name :: Name
      , date :: Maybe Day
      , description :: Description
      }
  | Complex
      { name :: Name
      , date :: Maybe Day
      , description :: Description
      , children :: [Task]
      }
  deriving (Show)

taskToString :: Task -> Integer -> String
taskToString (Complex name day desc subtasks) indentation =
  taskToString (Simple name day desc) indentation ++ childrenTasks
  where
    childrenTasks =
      concat
        ["\n\n" ++ taskToString subtask (indentation + 1) | subtask <- subtasks]
taskToString (Simple name day desc) indentation =
  case day of
    Nothing -> indent ++ name ++ "\n\t" ++ indent ++ desc
    Just day ->
      indent ++ name ++ ": " ++ dayToString day ++ "\n\t" ++ indent ++ desc
  where
    indent = concat ["\t" | _ <- [1 .. indentation]]

dayToString :: Day -> String
dayToString day = show d ++ "-" ++ show m ++ "-" ++ show y
  where
    (y, m, d) = toGregorian day
