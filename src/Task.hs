module Task
  ( Task(Simple, Complex)
  , State(Todo, Done, None)
  , Name
  , Description
  , Id
  , boolToState
  , taskToString
  , dayToString
  ) where

import Data.Time
import Data.Time.Calendar

type Name = String

type Description = String

type Id = Int

data State
  = Todo
  | Done
  | None

instance Show State where
  show Todo = "TODO: "
  show Done = "DONE: "
  show None = ""

boolToState :: Bool -> State
boolToState True = Todo
boolToState False = None

data Task
  = Simple
      { id :: Id
      , state :: State
      , name :: Name
      , date :: Maybe Day
      , description :: Description
      }
  | Complex
      { id :: Id
      , state :: State
      , name :: Name
      , date :: Maybe Day
      , description :: Description
      , children :: [Task]
      }

instance Show Task where
  show t = taskToString t 0

simple :: Task -> Task
simple (Complex id state name date description _) =
  Simple id state name date description

taskToString :: Task -> Integer -> String
taskToString (Simple id state name day desc) indentation =
  case day of
    Nothing ->
      indent ++
      "[" ++ show id ++ "] " ++ show state ++ name ++ ": \n\t" ++ indent ++ desc
    Just day ->
      indent ++
      "[" ++
      show id ++
      "] " ++
      show state ++ name ++ ": " ++ dayToString day ++ "\n\t" ++ indent ++ desc
  where
    indent = concat ["\t" | _ <- [1 .. indentation]]
taskToString task indentation =
  taskToString (simple task) indentation ++ childrenTasks
  where
    childrenTasks =
      concat
        [ "\n\n" ++ taskToString subtask (indentation + 1)
        | subtask <- children task
        ]

dayToString :: Day -> String
dayToString day = show d ++ "-" ++ show m ++ "-" ++ show y
  where
    (y, m, d) = toGregorian day
