{-# LANGUAGE OverloadedStrings #-}
module Builder.Note
  ( Note(..)
  , buildNotes
  , buildNote
  , FrontmatterParseResult
  , NoteFrontmatter
  , findNote
  )
where

import           Data.Text                      ( Text
                                                , pack
                                                )
import           Helpers.Helpers                ( readNoteRaw )
import           Data.Yaml
import           Parser.Frontmatter
import           Data.Maybe
import           Data.Either

data NoteFrontmatter = NoteFrontmatter {title' :: Maybe Text, tags' :: Maybe [Text]} deriving (Show)

instance FromJSON NoteFrontmatter where
  parseJSON = withObject "NoteFrontmatter"
    $ \v -> NoteFrontmatter <$> v .:? "title" <*> v .:? "tags"
type FrontmatterParseResult = Either String NoteFrontmatter

data Note = Note {
  title :: Text,
  filename :: FilePath,
  raw :: Text,
  tags :: [Text],
  identifier :: Text
} deriving Show

buildNotes :: [FilePath] -> IO [Note]
buildNotes = mapM buildNote

buildNote :: FilePath -> IO Note
buildNote notePath = do
  noteRaw <- readNoteRaw notePath
  let fm          = parseFrontmatterEither noteRaw :: FrontmatterParseResult
      pageContent = parseRestOfPageEither noteRaw
  return $ Note { title      = getNoteTitle notePath fm
                , raw        = fromRight noteRaw pageContent
                , filename   = notePath
                , tags       = getNoteTags fm
                , identifier = getTitleFromPath notePath
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

findNote :: Text -> [Note] -> Maybe Note
findNote needle haystack =
  let filterResult = filter (\n -> identifier n == needle) haystack
  in  case filterResult of
        []      -> Nothing
        (a : _) -> Just a
