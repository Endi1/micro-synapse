module Builder.Note
  ( Note(..)
  , buildNotes
  , buildNote
  )
where

import           Data.Text                      ( Text
                                                , pack
                                                )
import           CMark                          ( commonmarkToHtml )
import           Parser.Innerlinks              ( parseInnerLinksToHtml )
import           Helpers.Helpers                ( readNoteRaw )

data Note = Note {
  title :: Text,
  raw :: Text,
  html :: Text
} deriving Show

buildNotes :: [FilePath] -> IO [Note]
buildNotes = mapM buildNote

buildNote :: FilePath -> IO Note
buildNote notePath = do
  noteRaw  <- readNoteRaw notePath
  noteHtml <- parseInnerLinksToHtml $ commonmarkToHtml [] noteRaw

  return $ Note { title = getTitleFromPath notePath
                , raw   = noteRaw
                , html  = noteHtml
                }
 where
  getTitleFromPath :: FilePath -> Text
  getTitleFromPath = pack . reverse . drop 3 . reverse
