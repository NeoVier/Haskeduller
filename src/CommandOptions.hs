module CommandOptions
  ( Command(..)
  , ListOptions(..)
  , AddFields(..)
  , UpdateFields(..)
  , Opts
  ) where

import Data.Time
import Task

data Command
  = List ListOptions
  | Add AddFields
  | Remove Id
  | Update UpdateFields
  deriving (Show)

data ListOptions
  = Today
  | Tomorrow
  | OneDay Day
  | ThisWeek
  | NextWeek
  | OneWeek Day
  | WithoutDate
  | WithDate
  | All
  deriving (Show)

data AddFields =
  AddFields
    { aname :: Name
    , astate :: Bool
    , adate :: Day
    , adesc :: Description
    , apid :: Id
    }
  deriving (Show)

data UpdateFields =
  UpdateFields
    { uid :: String
    , uname :: Name
    , udate :: Day
    , udesc :: Description
    , ucycle :: Bool
    }
  deriving (Show)

type Opts = Command
