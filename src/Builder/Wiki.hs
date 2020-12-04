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
import           Data.Text                      ( Text )
import           Data.Text.Lazy                 ( toStrict )
import           Builder.Note                   ( Note(..)
                                                , buildNotes
                                                )
import           Lucid.Base                     ( renderText )
import           Builder.Tree                   ( buildTree
                                                , getOrphans
                                                )
import           Data.Tree                      ( Tree )

import           Clay                           ( render )
import           Builder.Templating.Templates   ( noteTemplate
                                                , treeTemplate
                                                )
import           Builder.Styling.Style          ( pageStyle )
import           Text.Printf



buildWiki :: IO ()
buildWiki = do
  markdownFiles <- getMarkdownFiles =<< getCurrentDirectory
  notes         <- buildNotes markdownFiles
  tree          <- buildTree notes
  createRes
  writeNotesToRes notes
  writeTreeToRes tree (getOrphans tree notes)

writeTreeToRes :: Tree Text -> [Text] -> IO ()
writeTreeToRes tree orphans = do
  currentDir <- getCurrentDirectory
  DTIO.writeFile (currentDir ++ "/.res/" ++ "tree.html")
                 (toStrict $ renderText $ treeTemplate tree orphans)

writeNotesToRes :: [Note] -> IO ()
writeNotesToRes notes = do
  currentDir <- getCurrentDirectory
  mapM_
    (\n -> DTIO.writeFile
      (printf "%s/.res/%s.html" currentDir (Builder.Note.filename n))
      (toStrict $ renderText $ noteTemplate n)
    )
    notes
  DTIO.writeFile (printf "%s/.res/style.css" currentDir)
                 (toStrict $ render pageStyle)

createRes :: IO ()
createRes = do
  currentDir  <- getCurrentDirectory
  dirContents <- listDirectory currentDir
  if ".res" `elem` dirContents
    then do
      removeDirectoryRecursive (currentDir ++ "/.res")
      createDirectory (currentDir ++ "/.res")
    else createDirectory (currentDir ++ "/.res")

getMarkdownFiles :: FilePath -> IO [FilePath]
getMarkdownFiles currentDir = Prelude.filter isMarkdownFile
  <$> listDirectory currentDir
 where
  isMarkdownFile :: FilePath -> Bool
  isMarkdownFile fileName = take 3 (Prelude.reverse fileName) == "dm."
