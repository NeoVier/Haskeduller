module Main where

import Data.Semigroup ((<>))
import Data.Time
import Data.Version (showVersion)
import Lib
import Options.Applicative
import Paths_Haskeduller (version)
import Task
import Testing

data Command
  = List Bool
  | Add Name Bool Day Description
  | Remove String
  | Update String String

type Opts = Command

main :: IO ()
main = do
  opts <- execParser optsParser
  case opts of
    List val -> putStrLn ("Listing tasks: " ++ show val)
    Add name state date description ->
      if date == fromGregorian 1 1 1
        then print (Simple 0 (boolToState state) name Nothing description)
        else print (Simple 0 (boolToState state) name (Just date) description)
    Remove name -> putStrLn ("Removing task: " ++ name)
    Update name newName -> putStrLn ("Updating " ++ name ++ " to " ++ newName)
  where
    optsParser :: ParserInfo Opts
    optsParser =
      info
        (helper <*> versionOption <*> programOptions)
        (fullDesc <>
         header "Haskeduller - an Org mode inspired TODO list manager")
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
      Add <$> strArgument (metavar "NAME" <> help "Name of the new task") <*>
      switch
        (long "state" <>
         short 's' <> help "Whether to mark it as TODO initially") <*>
      option
        dayReader
        (long "date" <>
         metavar "DATE" <>
         value (fromGregorian 1 1 1) <>
         help "Set the due date in the format %d-%m-%Y") <*>
      strOption
        (long "description" <>
         short 'd' <>
         metavar "DESCRIPTION" <> help "Give the task some description")
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

dayReader :: ReadM Day
dayReader =
  eitherReader $ \arg ->
    case parseTimeM True defaultTimeLocale "%d-%m-%Y" arg of
      Nothing ->
        Left
          ("Cannot parse date:" ++
           arg ++ "\n Make sure it's in the format %d-%m-%Y")
      Just day -> Right day
