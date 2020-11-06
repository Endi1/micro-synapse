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
import           Helpers.Helpers

parseInnerLinks :: Text -> [Text]
parseInnerLinks noteRaw =
  let parseResult = A.parseOnly (A.many1 parseLinks) noteRaw
  in  case parseResult of
        Left  _           -> []
        Right linkSymbols -> linkSymbols

parseInnerLinksToHtml :: Text -> IO Text
parseInnerLinksToHtml noteHtml =
  let parseResult = A.parseOnly (A.many1 parseLinks) noteHtml
  in  case parseResult of
        Left  _           -> return noteHtml
        Right linkSymbols -> replaceLinks linkSymbols noteHtml
 where
  replaceLinks :: [Text] -> Text -> IO Text
  replaceLinks []       noteHtml = return noteHtml
  replaceLinks (x : xs) noteHtml = do
    noteRaw <- readNoteRaw (unpack x ++ ".md")
    let titleParseResult = A.parseOnly parseTitle noteRaw
    case titleParseResult of
      Left  _     -> replaceLinks xs $ replaceLink x x noteHtml
      Right title -> replaceLinks xs $ replaceLink x title noteHtml

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
