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
  , toComplex
  , isComplex
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
  deriving (Eq)

instance Show State where
  show Todo = "TODO"
  show Done = "DONE"
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
  deriving (Eq)

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

toComplex :: Task -> Task
toComplex (Simple identifier state name date description) =
  Complex
    { identifier = identifier
    , state = state
    , name = name
    , date = date
    , description = description
    , children = []
    }
toComplex t = t

isComplex :: Task -> Bool
isComplex t = toComplex t == t

taskFactory ::
     Id -> State -> Name -> Maybe Day -> Description -> Maybe [Task] -> Task
taskFactory id state name date description children =
  case children of
    Nothing -> Simple id state name date description
    (Just subtasks) -> Complex id state name date description subtasks

changeId :: Task -> Id -> Task
changeId task id = task {identifier = id}

displayState :: State -> String
displayState None = ""
displayState s = show s ++ ": "

taskToString :: Task -> Integer -> String
taskToString (Simple identifier state name day desc) indentation =
  case day of
    Nothing ->
      indent ++
      "[" ++
      identifier ++
      "] " ++ displayState state ++ name ++ ": \n\t" ++ indent ++ desc
    Just day ->
      indent ++
      "[" ++
      identifier ++
      "] " ++
      displayState state ++
      name ++ ": " ++ dayToString day ++ "\n\t" ++ indent ++ desc
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
dayToString day
  | day == fromGregorian 1 1 1 = ""
  | otherwise = padNum d ++ "/" ++ padNum m ++ "/" ++ padNum y
  where
    (y, m, d) = toGregorian day
    padNum x
      | x < 10 = "0" ++ show x
      | otherwise = show x
