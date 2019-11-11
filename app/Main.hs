module Main where

import Data.Semigroup ((<>))
import Data.Time
import Data.Version (showVersion)
import Lib
import Options.Applicative
import Paths_Haskeduller (version) -- Displays same version as in package.yaml
import Task
import Testing

data Command
  = List Bool
  | Add String
  | Remove String
  | Update String String

type Opts = Command

main :: IO ()
main = do
  opts <- execParser optsParser
  case opts of
    List val -> putStrLn ("Listing tasks: " ++ show val)
    Add name -> putStrLn ("Adding new task: " ++ name)
    Remove name -> putStrLn ("Removing task: " ++ name)
    Update name newName -> putStrLn ("Updating " ++ name ++ " to " ++ newName)
  where
    optsParser :: ParserInfo Opts
    optsParser =
      info
        (helper <*> versionOption <*> programOptions)
        (fullDesc <>
         progDesc "optparse subcommands example" <>
         header "Haskeduller - an Org mode inspired TODO list")
    versionOption :: Parser (a -> a)
    versionOption =
      infoOption (showVersion version) (long "version" <> help "Show version")
    programOptions :: Parser Opts
    programOptions =
      hsubparser (listCommand <> addCommand <> removeCommand <> updateCommand)
    listCommand :: Mod CommandFields Command
    listCommand =
      command "list" (info listOptions (progDesc "Display task list"))
    listOptions :: Parser Command
    listOptions =
      List <$> switch (long "today" <> short 't' <> help "List today's tasks")
    addCommand :: Mod CommandFields Command
    addCommand = command "add" (info addOptions (progDesc "Add a new task"))
    addOptions :: Parser Command
    addOptions =
      Add <$> strArgument (metavar "NAME" <> help "Name of the new task")
    removeCommand :: Mod CommandFields Command
    removeCommand =
      command "remove" (info removeOptions (progDesc "Remove a task"))
    removeOptions :: Parser Command
    removeOptions =
      Remove <$>
      strArgument (metavar "NAME" <> help "Name of the task to remove")
    updateCommand :: Mod CommandFields Command
    updateCommand =
      command
        "update"
        (info
           updateOptions
           (progDesc "Change the attributes of an existing task"))
    updateOptions :: Parser Command
    updateOptions =
      Update <$>
      strArgument (metavar "NAME" <> help "Name of the existing task") <*>
      strArgument (metavar "NEW NAME" <> help "New name of the task")
