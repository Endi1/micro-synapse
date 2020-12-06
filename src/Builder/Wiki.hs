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
import           CMark                          ( commonmarkToHtml )
import           Parser.Innerlinks              ( parseInnerLinksToHtml )
import           Builder.Types                  ( NoteAndHTML
                                                , NoteHTML
                                                )


buildWiki :: IO ()
buildWiki = do
  markdownFiles <- getMarkdownFiles =<< getCurrentDirectory
  notes         <- buildNotes markdownFiles
  notesRendered <- mapM
    (\n -> do
      noteHTML <- renderNoteHTML (raw n) notes
      return (n, noteHTML)
    )
    notes
  tree <- buildTree notes
  createRes
  writeNotesToRes notesRendered
  writeTreeToRes tree (getOrphans tree notes)

writeTreeToRes :: Tree Text -> [Text] -> IO ()
writeTreeToRes tree orphans = do
  currentDir <- getCurrentDirectory
  DTIO.writeFile (currentDir ++ "/.res/" ++ "tree.html")
                 (toStrict $ renderText $ treeTemplate tree orphans)

writeNotesToRes :: [NoteAndHTML] -> IO ()
writeNotesToRes notesRendered = do
  currentDir <- getCurrentDirectory
  mapM_ writeNoteToRes notesRendered


writeNoteToRes :: NoteAndHTML -> IO ()
writeNoteToRes (note, noteHTML) = do
  currentDir <- getCurrentDirectory
  DTIO.writeFile
    (printf "%s/.res/%s.html" currentDir (Builder.Note.identifier note))
    (toStrict $ renderText $ noteTemplate note noteHTML)

renderNoteHTML :: Text -> [Note] -> IO NoteHTML
renderNoteHTML noteRaw = parseInnerLinksToHtml $ commonmarkToHtml [] noteRaw

createRes :: IO ()
createRes = do
  currentDir  <- getCurrentDirectory
  dirContents <- listDirectory currentDir
  if ".res" `elem` dirContents
    then do
      removeDirectoryRecursive (currentDir ++ "/.res")
      createDirectory (currentDir ++ "/.res")
    else createDirectory (currentDir ++ "/.res")
  DTIO.writeFile (printf "%s/.res/style.css" currentDir)
                 (toStrict $ render pageStyle)

getMarkdownFiles :: FilePath -> IO [FilePath]
getMarkdownFiles currentDir = Prelude.filter isMarkdownFile
  <$> listDirectory currentDir
 where
  isMarkdownFile :: FilePath -> Bool
  isMarkdownFile fileName = take 3 (Prelude.reverse fileName) == "dm."
