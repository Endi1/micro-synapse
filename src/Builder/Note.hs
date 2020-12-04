{-# LANGUAGE OverloadedStrings #-}
module Builder.Note
  ( Note(..)
  , buildNotes
  , buildNote
  , FrontmatterParseResult
  , NoteFrontmatter
  )
where

import           Data.Text                      ( Text
                                                , pack
                                                )
import           CMark                          ( commonmarkToHtml )
import           Parser.Innerlinks              ( parseInnerLinksToHtml )
import           Helpers.Helpers                ( readNoteRaw )
import           GHC.Generics
import           Data.Yaml
import           Parser.Frontmatter
import           Data.Maybe

data NoteFrontmatter = NoteFrontmatter {title' :: Maybe Text, tags' :: Maybe [Text]} deriving (Show)

instance FromJSON NoteFrontmatter where
  parseJSON = withObject "NoteFrontmatter"
    $ \v -> NoteFrontmatter <$> v .:? "title" <*> v .:? "tags"
type FrontmatterParseResult = Either String NoteFrontmatter

data Note = Note {
  title :: Text,
  filename :: FilePath,
  raw :: Text,
  html :: Text,
  tags :: [Text]
} deriving Show

buildNotes :: [FilePath] -> IO [Note]
buildNotes = mapM buildNote

buildNote :: FilePath -> IO Note
buildNote notePath = do
  noteRaw  <- readNoteRaw notePath
  noteHtml <- parseInnerLinksToHtml $ commonmarkToHtml [] noteRaw
  let fm = parseFrontmatterEither noteRaw :: FrontmatterParseResult
  return $ Note { title    = getNoteTitle notePath fm
                , raw      = noteRaw
                , filename = notePath
                , html     = noteHtml
                , tags     = getNoteTags fm
                }

getTitleFromPath :: FilePath -> Text
getTitleFromPath = pack . reverse . drop 3 . reverse

getNoteTitle :: FilePath -> FrontmatterParseResult -> Text
getNoteTitle notePath fm = case fm of
  Right a -> fromMaybe (getTitleFromPath notePath) (title' a)
  Left  _ -> getTitleFromPath notePath

getNoteTags :: FrontmatterParseResult -> [Text]
getNoteTags fm = case fm of
  Left  _ -> []
  Right a -> fromMaybe [] (tags' a)
