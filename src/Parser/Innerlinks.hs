{-# LANGUAGE OverloadedStrings #-}
module Parser.Innerlinks
  ( parseInnerLinksToHtml
  , parseInnerLinks
  )
where

import           Data.Text                      ( Text
                                                , pack
                                                , replace
                                                , unpack
                                                )
import qualified Data.Attoparsec.Text          as A
import           Text.Printf                    ( printf )
import           Options.Applicative            ( Alternative((<|>)) )
import           Builder.Note
import           Builder.Types

parseInnerLinks :: Text -> [Text]
parseInnerLinks noteRaw =
  let filenameInLinksParseResult = A.parseOnly (A.many1 parseLinks) noteRaw
  in  case filenameInLinksParseResult of
        Left  _         -> []
        Right filenames -> filenames

parseInnerLinksToHtml :: NoteHTML -> [Note] -> IO Text
parseInnerLinksToHtml noteHtml allNotes =
  let filenameInLinksParseResult = A.parseOnly (A.many1 parseLinks) noteHtml
  in  case filenameInLinksParseResult of
        Left  _               -> return noteHtml
        Right filenameInLinks -> replaceLinks filenameInLinks noteHtml allNotes
 where
  replaceLinks :: [Text] -> Text -> [Note] -> IO Text
  replaceLinks []       noteHtml _        = return noteHtml
  replaceLinks (x : xs) noteHtml allNotes = do
    let noteWithFileName = findNote x allNotes
    case noteWithFileName of
      Nothing -> replaceLinks xs (replaceLink x x noteHtml) allNotes
      Just note ->
        replaceLinks xs (replaceLink x (title note) noteHtml) allNotes

  replaceLink :: Text -> Text -> Text -> Text
  replaceLink filename title = replace
    (pack (printf "[[%s]]" filename :: String))
    (pack (printf "<a href='%s.html'>%s</a>" filename title :: String))



parseLinks :: A.Parser Text
parseLinks = do
  A.takeTill (== '[')
  parseLink <|> continue
 where
  continue = do
    A.char '['
    parseLinks


parseLink :: A.Parser Text
parseLink = do
  A.string "[["
  link <- A.many1 (A.letter <|> A.char '_')
  A.string "]]"
  return $ pack link

parseTitle :: A.Parser Text
parseTitle = do
  A.takeWhile (== ' ')
  A.char '#'
  A.takeWhile (== ' ')
  title <- A.many1 (A.letter <|> A.char ' ')
  A.endOfLine
  return $ pack title
