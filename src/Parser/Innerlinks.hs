{-# LANGUAGE OverloadedStrings #-}
module Parser.Innerlinks
  ( parseInnerLinksToHtml
  , parseInnerLinks
  )
where

import           Data.Text                      ( Text
                                                , pack
                                                , replace
                                                )
import qualified Data.Attoparsec.Text          as A
import           Text.Printf                    ( printf )
import           Options.Applicative            ( Alternative((<|>)) )

parseInnerLinks :: Text -> [Text]
parseInnerLinks noteRaw =
  let parseResult = A.parseOnly (A.many1 parseLinks) noteRaw
  in  case parseResult of
        Left  _           -> []
        Right linkSymbols -> linkSymbols

parseInnerLinksToHtml :: Text -> Text
parseInnerLinksToHtml noteHtml =
  let parseResult = A.parseOnly (A.many1 parseLinks) noteHtml
  in  case parseResult of
        Left  _           -> noteHtml
        Right linkSymbols -> replaceLinks linkSymbols noteHtml
 where
  replaceLinks :: [Text] -> Text -> Text
  replaceLinks []       noteHtml = noteHtml
  replaceLinks (x : xs) noteHtml = replaceLinks xs $ replace
    (pack (printf "[[%s]]" x :: String))
    (pack (printf "<a href='%s.html'>%s</a>" x x :: String))
    noteHtml



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
