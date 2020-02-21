# Haskeduller

Haskeduller is an [Org mode](https://orgmode.org) inspired TODO list manager.

## Getting started

### Prerequisites

* [Stack](https://docs.haskellstack.org/en/stable/README/)

### Installing

To get Haskeduller, simply clone this repository by doing `git clone https://github.com/NeoVier/Haskeduller` or manually cloning through GitHub.
To have the binary in one of your `$PATH` directories, run `stack build --copy-bins` inside the cloned directory.

## Usage

Haskeduller uses `$HOME/.haskeduller.json` as the file for holding all of the tasks. Currently, it only supports this file.
If you have [installed](#Installing) Haskeduller, all you need to do to run it is run the command `haskeduller COMMAND`.
For further information, you can run `haskeduller -h`, `haskeduller --help` or `haskeduller COMMAND --help`.

### List tasks

The list command simply prints out the tasks specified by the argument given, in a hierarchical way.
Usage: `haskeduller list COMMAND`

A task is displayed as such:

```
[Id] (TODO/DONE): (NAME): (DATE)
	(Description)
	(Children)
```

#### Commands

* today: Lists all tasks marked for the current day;
* tomorrow: Lists all tasks marked for the next day;
* day DATE: Lists all tasks marked for DATE, which is specified in the format `%d/%m/%Y`;
* current: Lists all tasks marked for the current week;
* next: Lists all tasks marked for next week;
* week DATE: Lists all tasks marked for the week of DATE. DATE can be any day within the desired week, and is also in the format `%d/%m/%Y`;
* without: Lists all tasks that aren't associated with dates;
* with: Lists all tasks that are associated with dates;
* all: Lists all tasks.


### Add tasks

The add command adds a new task to the list.
Usage: `haskeduller add NAME [-s|--state] [--date DATE] [-d|--description DESCRIPTION] [-p|--parent PARENT_ID]`

#### Options

* NAME: The name of the task;
* state: Whether or not to mark it as TODO initially;
* date DATE: The date the task has to be done by, specified in the format `%d/%m/%Y`;
* description DESCRIPTION: A short description of the task;
* parent PARENT_ID: If set, this task will be marked as a child of PARENT_ID.

### Remove tasks

The remove command removes a task and all of its children, if it has any.
Usage: `haskduller remove ID`

#### Options

* ID: The Id of the task to be removed.

### Update tasks

The update command is used to change one or more fields of a task.
Usage: `haskeduller update ID [-n|--name NEW_NAME] [--date NEW_DATE] [-d|--description NEW_DESCRIPTION] [-c|--cycle]`

#### Options

* ID: Id of the task to be updated;
* name NEW_NAME: The new name of the task;
* date NEW_DATE: The new date of the task;
* description NEW_DESCRIPTION: the new description of the task;
* cycle: Whether or not to change the state of the task. It goes in the circular order TODO -> DONE -> No state.

## Future Plans

In the future, I plan to support multiple tracked files, with one main file. This way, you could organize different activities in different files.
