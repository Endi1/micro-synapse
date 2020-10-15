{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Cli.Commands                   ( Commands(Commands)
                                                , commands
                                                )
import           Options.Applicative            ( (<**>)
                                                , fullDesc
                                                , info
                                                , progDesc
                                                , execParser
                                                , helper
                                                )

import           Builder.Wiki                   ( buildWiki )
import           Web.Server                     ( runServer )


main :: IO ()
main = exec =<< execParser opts where
  opts = info (commands <**> helper)
              (fullDesc <> progDesc "Build a wiki from your notes")


exec :: Commands -> IO ()
exec (Commands True "" _    _    ) = putStrLn "Creating note with random title"
exec (Commands True a  _    _    ) = putStrLn $ "Creating note called " ++ a
exec (Commands _    _  True False) = buildWiki
exec (Commands _    _  _    True ) = runServer
