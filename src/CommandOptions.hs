module CommandOptions
  ( Command(..)
  , ListOptions(..)
  , Opts
  ) where

import Data.Time
import Task

data Command
  = List ListOptions
  | Add Name Bool Day Description Id
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
  | All
  deriving (Show)

type Opts = Command
