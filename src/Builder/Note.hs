module Builder.Note
  ( Note(..)
  , buildNotes
  , buildNote
  )
where

import           Data.Text                      ( Text
                                                , pack
                                                )
import qualified Data.Text.IO                  as DTIO
import           CMark                          ( commonmarkToHtml )
import           System.Directory               ( getCurrentDirectory )
import           Parser.Innerlinks              ( parseInnerLinksToHtml )

data Note = Note {
  title :: Text,
  raw :: Text,
  html :: Text
} deriving Show

buildNotes :: [FilePath] -> IO [Note]
buildNotes = mapM buildNote

buildNote :: FilePath -> IO Note
buildNote notePath = do
  currentDir <- getCurrentDirectory
  noteRaw    <- DTIO.readFile (currentDir ++ "/" ++ notePath)
  let noteHtml = commonmarkToHtml [] noteRaw
  return $ Note { title = getTitleFromPath notePath
                , raw   = noteRaw
                , html  = parseInnerLinksToHtml noteHtml
                }
 where
  getTitleFromPath :: FilePath -> Text
  getTitleFromPath = pack . reverse . drop 3 . reverse
