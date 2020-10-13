module Builder.Wiki
  ( buildWiki
  )
where

import qualified Data.Text.IO                  as DTIO
import           System.Directory               ( createDirectory
                                                , getCurrentDirectory
                                                , listDirectory
                                                , removeDirectoryRecursive
                                                )
import           Data.Text                      ( unpack )
import           Builder.Note                   ( Note(..)
                                                , buildNotes
                                                )


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

getMarkdownFiles :: FilePath -> IO [FilePath]
getMarkdownFiles currentDir = filter isMarkdownFile
  <$> listDirectory currentDir
 where
  isMarkdownFile :: FilePath -> Bool
  isMarkdownFile fileName = take 3 (reverse fileName) == "dm."
