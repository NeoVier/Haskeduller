module Main where

import CommandOptions
import Data.Semigroup ((<>))
import Data.Time
import Data.Version (showVersion)
import Lib
import Options.Applicative
import Paths_Haskeduller (version)
import Reading.JsonHelper
import Task
import Testing
import Writing.TaskToJson

main :: IO ()
main = do
  opts <- execParser optsParser
  case opts of
    List opt -> execList "test/sched.json" opt
    Add addFields -> execAdd "test/sched.json" addFields
    Remove id -> putStrLn ("Removing task: " ++ id)
    Update id new_name new_day new_description cycle ->
      putStrLn
        (id ++
         " " ++
         new_name ++
         " " ++ show new_day ++ " " ++ new_description ++ " " ++ show cycle)
  where
    optsParser :: ParserInfo Opts
    optsParser =
      info
        (helper <*> versionOption <*> programOptions)
        (fullDesc <>
         header "Haskeduller - an Org mode inspired TODO list manager.")
    versionOption :: Parser (a -> a)
    versionOption =
      infoOption (showVersion version) (long "version" <> help "Show version.")
    programOptions :: Parser Opts
    programOptions =
      hsubparser (listCommand <> addCommand <> removeCommand <> updateCommand)

listCommand :: Mod CommandFields Command
listCommand = command "list" (info listOptions (progDesc "Display task list."))

addCommand :: Mod CommandFields Command
addCommand = command "add" (info addOptions (progDesc "Add a new task."))

removeCommand :: Mod CommandFields Command
removeCommand =
  command
    "remove"
    (info removeOptions (progDesc "Remove a task and all of its children."))

updateCommand :: Mod CommandFields Command
updateCommand =
  command
    "update"
    (info updateOptions (progDesc "Change the attributes of an existing task."))

listOptions :: Parser Command
listOptions =
  List <$>
  hsubparser
    (today <>
     tomorrow <>
     day <> week <> nextWeek <> oneWeek <> withoutDate <> withDate <> listAll)
  where
    today :: Mod CommandFields ListOptions
    today =
      command
        "today"
        (info (pure Today) (progDesc "List all tasks marked for today."))
    tomorrow :: Mod CommandFields ListOptions
    tomorrow =
      command
        "tomorrow"
        (info (pure Tomorrow) (progDesc "List all tasks marked for tomorrow."))
    day :: Mod CommandFields ListOptions
    day =
      command
        "day"
        (info dayOptions (progDesc "List all tasks marked for a certain date."))
    dayOptions :: Parser ListOptions
    dayOptions =
      OneDay <$>
      argument
        dayReader
        (metavar "DATE" <> help "Target date in the format %d/%m/%Y.")
    week :: Mod CommandFields ListOptions
    week =
      command
        "current"
        (info
           (pure ThisWeek)
           (progDesc "List all tasks marked for the current week."))
    nextWeek :: Mod CommandFields ListOptions
    nextWeek =
      command
        "next"
        (info
           (pure NextWeek)
           (progDesc "List all tasks marked for the next week."))
    oneWeek :: Mod CommandFields ListOptions
    oneWeek =
      command
        "week"
        (info
           oneWeekOptions
           (progDesc "List all tasks marked within a specific week."))
    oneWeekOptions :: Parser ListOptions
    oneWeekOptions =
      OneWeek <$>
      argument
        dayReader
        (metavar "DATE" <> help "Any date within the target week.")
    withoutDate :: Mod CommandFields ListOptions
    withoutDate =
      command
        "without"
        (info
           (pure WithoutDate)
           (progDesc "List all tasks that aren't associated with dates."))
    withDate :: Mod CommandFields ListOptions
    withDate =
      command
        "with"
        (info
           (pure WithDate)
           (progDesc "List all tasks that are associated with dates."))
    listAll :: Mod CommandFields ListOptions
    listAll = command "all" (info (pure All) (progDesc "List all tasks."))

addOptions :: Parser Command
addOptions =
  Add <$>
  (Fields <$> strArgument (metavar "NAME" <> help "Name of the new task.") <*>
   switch
     (long "state" <> short 's' <> help "Whether to mark it as TODO initially.") <*>
   option
     dayReader
     (long "date" <>
      metavar "DATE" <>
      value (fromGregorian 1 1 1) <>
      help "Set the due date in the format %d-%m-%Y.") <*>
   strOption
     (long "description" <>
      short 'd' <>
      value "" <>
      metavar "DESCRIPTION" <> help "Give the task some description.") <*>
   strOption
     (long "parent" <>
      short 'p' <>
      value "" <>
      metavar "PARENT ID" <> help "Set this task as child of PARENT ID."))

removeOptions :: Parser Command
removeOptions =
  Remove <$> strArgument (metavar "ID" <> help "ID of the task to remove.")

updateOptions :: Parser Command
updateOptions =
  Update <$> strArgument (metavar "ID" <> help "ID of the task to update.") <*>
  strOption
    (long "name" <>
     short 'n' <> value "" <> metavar "NEW NAME" <> help "New task name.") <*>
  option
    dayReader
    (long "date" <>
     metavar "NEW DATE" <>
     value (fromGregorian 1 1 1) <>
     help "Set new due date in the format %d/%m/%Y.") <*>
  strOption
    (long "description" <>
     short 'd' <>
     value "" <> metavar "DESCRIPTION" <> help "New task description.") <*>
  switch (long "cycle" <> short 'c' <> help "Cycle through TODO states.")

dayReader :: ReadM Day
dayReader =
  eitherReader $ \arg ->
    case parseTimeM True defaultTimeLocale "%d/%m/%Y" arg of
      Nothing ->
        Left
          ("Cannot parse date: " ++
           arg ++ "\n Make sure it's in the format %d/%m/%Y.")
      Just day -> Right day
