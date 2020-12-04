{-# LANGUAGE OverloadedStrings #-}
module Parser.Frontmatter
  ( parseFrontmatterEither
  )
where

import qualified Data.Attoparsec.Text          as A
import           Data.Text
import           Data.Yaml
import           Data.Text.Encoding

parseFrontmatterEither :: FromJSON a => Text -> Either String a
parseFrontmatterEither txt = A.eitherResult $ parseFrontmatterYaml txt

parseFrontmatterYaml :: FromJSON a => Text -> A.Result a
parseFrontmatterYaml = A.parse yamlParser

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
