module Main where

import Lib

import Task
import Testing

main :: IO ()
main = print $ taskToString superTask 0
