module CommandOptions
  ( Command(..)
  , ListOptions(..)
  , AddFields(..)
  , Opts
  ) where

import Data.Time
import Task

data Command
  = List ListOptions
  | Add AddFields
  | Remove Id
  | Update Id Name Day Description Bool
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
  Fields
    { fname :: Name
    , fstate :: Bool
    , fdate :: Day
    , fdesc :: Description
    , fpid :: Id
    }
  deriving (Show)

type Opts = Command
