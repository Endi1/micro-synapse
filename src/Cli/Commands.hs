module Cli.Commands
  ( Commands(..)
  , commands
  )
where

import           Options.Applicative            ( help
                                                , long
                                                , showDefault
                                                , strOption
                                                , switch
                                                , value
                                                , Parser
                                                )


data Commands = Commands {
  new :: Bool,
  name :: String,
  build :: Bool,
  run :: Bool
} deriving Show

commands :: Parser Commands
commands =
  Commands
    <$> switch (long "new" <> showDefault <> help "Create new note")
    <*> strOption
          (long "name" <> showDefault <> value "" <> help "Name for new file")
    <*> switch (long "build" <> showDefault <> help "Build wiki")
    <*> switch (long "run" <> showDefault <> help "Run server")

