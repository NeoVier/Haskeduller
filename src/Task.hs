module Task
  ( Task(..)
  , taskFactory
  , State(..)
  , Name
  , Description
  , Id
  , boolToState
  , taskToString
  , dayToString
  , changeId
  , filePath
  ) where

import Data.Time
import Data.Time.Calendar

type Name = String

type Description = String

type Id = String

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
      { identifier :: Id
      , state :: State
      , name :: Name
      , date :: Maybe Day
      , description :: Description
      }
  | Complex
      { identifier :: Id
      , state :: State
      , name :: Name
      , date :: Maybe Day
      , description :: Description
      , children :: [Task]
      }

instance Show Task where
  show t = taskToString t 0

simple :: Task -> Task
simple (Complex identifier state name date description _) =
  Simple
    { identifier = identifier
    , state = state
    , name = name
    , date = date
    , description = description
    }

taskFactory ::
     Id -> State -> Name -> Maybe Day -> Description -> Maybe [Task] -> Task
taskFactory id state name date description children =
  case children of
    Nothing -> Simple id state name date description
    (Just subtasks) -> Complex id state name date description subtasks

changeId :: Task -> Id -> Task
changeId task id = task {identifier = id}

taskToString :: Task -> Integer -> String
taskToString (Simple identifier state name day desc) indentation =
  case day of
    Nothing ->
      indent ++
      "[" ++
      identifier ++ "] " ++ show state ++ name ++ ": \n\t" ++ indent ++ desc
    Just day ->
      indent ++
      "[" ++
      identifier ++
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

filePath :: FilePath
filePath = "/home/henrique/SCHED"
------------------------------ File handling -----------------------------------
-- Move to another file?
  -- Could use readFile, writeFile
{-today :: IO String
today = do
  day <- utctDay <$> getCurrentTime
  return (dayToString day)-}
