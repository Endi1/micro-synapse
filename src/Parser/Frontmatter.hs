{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Parser.Frontmatter
  ( parseFrontmatterEither
  , parseRestOfPageEither
  , pageContentParser
  )
where

import qualified Data.Attoparsec.Text          as A
import           Data.Text
import           Data.Yaml
import           Data.Text.Encoding
import           GHC.Generics

newtype TestData = TestData {title :: Text} deriving (Show, FromJSON, Generic)

parseFrontmatterEither :: FromJSON a => Text -> Either String a
parseFrontmatterEither txt = A.eitherResult $ parseFrontmatterYaml txt

parseRestOfPageEither :: Text -> Either String Text
parseRestOfPageEither = A.parseOnly pageContentParser


parseFrontmatterYaml :: FromJSON a => Text -> A.Result a
parseFrontmatterYaml = A.parse yamlParser

pageContentParser :: A.Parser Text
pageContentParser = do
  _           <- A.skipMany frontmatter
  restOfInput <- A.manyTill A.anyChar A.endOfInput
  return $ pack restOfInput

yamlParser :: FromJSON a => A.Parser a
yamlParser = do
  f <- frontmatter
  case decodeEither' $ encodeUtf8 f of
    Left  e -> fail $ show e
    Right v -> return v

frontmatter :: A.Parser Text
frontmatter = do
  f <- frontmatterSeparator *> A.manyTill A.anyChar frontmatterSeparator
  return $ pack f

frontmatterSeparator :: A.Parser ()
frontmatterSeparator = A.string "---" >> A.endOfLine
