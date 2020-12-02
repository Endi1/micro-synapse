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
import           System.Random                  ( newStdGen
                                                , Random(randomRs)
                                                )
import           System.IO.Unsafe               ( unsafePerformIO )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           System.Directory

randomStr :: Text
randomStr = pack $ take 6 $ randomRs ('a', 'z') $ unsafePerformIO newStdGen

createNoteFile :: Text -> IO ()
createNoteFile noteTitle = do
  currentDir <- getCurrentDirectory
  writeFile (currentDir ++ "/" ++ unpack noteTitle ++ ".md") ""


main :: IO ()
main = exec =<< execParser opts where
  opts = info (commands <**> helper)
              (fullDesc <> progDesc "Build a wiki from your notes")


exec :: Commands -> IO ()
exec (Commands True "" _    _    ) = createNoteFile randomStr
exec (Commands True a  _    _    ) = createNoteFile $ pack a
exec (Commands _    _  True False) = buildWiki
exec (Commands _    _  _    True ) = runServer
