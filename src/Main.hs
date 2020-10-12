{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Options.Applicative            ( (<**>)
                                                , fullDesc
                                                , help
                                                , info
                                                , long
                                                , progDesc
                                                , showDefault
                                                , strOption
                                                , switch
                                                , value
                                                , execParser
                                                , helper
                                                , Parser
                                                )
import           System.Directory               ( createDirectory
                                                , getCurrentDirectory
                                                , listDirectory
                                                , removeDirectoryRecursive
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import qualified Data.Text.IO                  as DTIO
import           CMark                          ( commonmarkToHtml )
import           Happstack.Server               ( serveDirectory
                                                , logMAccess
                                                , simpleHTTP
                                                , Browsing(EnableBrowsing)
                                                , Conf
                                                  ( Conf
                                                  , port
                                                  , validator
                                                  , logAccess
                                                  , timeout
                                                  , threadGroup
                                                  )
                                                )

data Commands = Commands {
  new :: Bool,
  name :: String,
  build :: Bool,
  run :: Bool
}

data Note = Note {
  title :: Text,
  raw :: Text,
  html :: Text
} deriving Show

runServer :: IO ()
runServer = do
  buildWiki
  simpleHTTP Conf { port        = 3000
                  , validator   = Nothing
                  , logAccess   = Just logMAccess
                  , timeout     = 3000
                  , threadGroup = Nothing
                  }
    $ serveDirectory EnableBrowsing ["index.html"] ".res"


buildWiki :: IO ()
buildWiki = do
  markdownFiles <- getMarkdownFiles =<< getCurrentDirectory
  notes         <- buildNotes markdownFiles
  writeToRes notes

writeToRes :: [Note] -> IO ()
writeToRes notes = do
  currentDir  <- getCurrentDirectory
  dirContents <- listDirectory currentDir
  if ".res" `elem` dirContents
    then do
      removeDirectoryRecursive (currentDir ++ "/.res")
      createDirectory (currentDir ++ "/.res")
    else createDirectory (currentDir ++ "/.res")

  mapM_
    (\n -> DTIO.writeFile
      (currentDir ++ "/.res/" ++ unpack (title n) ++ ".html")
      (html n)
    )
    notes

buildNotes :: [FilePath] -> IO [Note]
buildNotes = mapM buildNote

buildNote :: FilePath -> IO Note
buildNote notePath = do
  currentDir <- getCurrentDirectory
  noteRaw    <- DTIO.readFile (currentDir ++ "/" ++ notePath)
  return $ Note { title = getTitleFromPath notePath
                , raw   = noteRaw
                , html  = commonmarkToHtml [] noteRaw
                }
 where
  getTitleFromPath :: FilePath -> Text
  getTitleFromPath = pack . reverse . drop 3 . reverse

getMarkdownFiles :: FilePath -> IO [FilePath]
getMarkdownFiles currentDir = filter isMarkdownFile
  <$> listDirectory currentDir
 where
  isMarkdownFile :: FilePath -> Bool
  isMarkdownFile fileName = take 3 (reverse fileName) == "dm."

commands :: Parser Commands
commands =
  Commands
    <$> switch (long "new" <> showDefault <> help "Create new note")
    <*> strOption
          (long "name" <> showDefault <> value "" <> help "Name for new file")
    <*> switch (long "build" <> showDefault <> help "Build wiki")
    <*> switch (long "run" <> showDefault <> help "Run server")


main :: IO ()
main = exec =<< execParser opts where
  opts = info (commands <**> helper)
              (fullDesc <> progDesc "Build a wiki from your notes")

exec :: Commands -> IO ()
-- exec (Commands False _ False False) = putStrLn "We're not creating a new note"
-- exec (Commands True filename _ _) =
--   putStrLn $ "Creating a note called " ++ filename
exec (Commands _ _ _ True) = runServer
-- exec (Commands _     _ True False) = buildWiki
