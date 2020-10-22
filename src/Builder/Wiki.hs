{-# LANGUAGE OverloadedStrings #-}
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
import           Data.Text                      ( unpack
                                                , Text
                                                , append
                                                )
import           Data.Text.Lazy                 ( toStrict )
import           Builder.Note                   ( Note(..)
                                                , buildNotes
                                                )
import           Lucid.Base
import           Lucid.Html5
import           Builder.Tree
import           Data.Tree

import           Clay

primaryColor :: Color
primaryColor = "#1947BD"

blackColor :: Color
blackColor = "#1E212B"

dangerColor :: Color
dangerColor = "#ED254E"

successColor :: Color
successColor = "#97CC04"

containerCss :: Css
containerCss = ".container" ? do
  display flex
  justifyContent center
  marginLeft $ em 15
  marginRight $ em 20

styling :: Css
styling = do
  Clay.html ? do
    color blackColor
    fontSize $ px 20
    fontFamily [] [sansSerif]
    background (lighten 0.9 blackColor)
  containerCss
  ul ? do
    listStyle none outside none
  a ? do
    textDecorationLine none
    color primaryColor
    ":hover" & do
      color (lighten 0.5 primaryColor)

template :: Note -> Html ()
template note = html_ $ do
  head_ $ do
    title_ "Endi's Wiki"
    link_ [rel_ "stylesheet", href_ "./style.css"]
  body_ $ do
    div_ [class_ "container"] $ do
      div_ [class_ "note"] $ do
        toHtmlRaw $ Builder.Note.html note

treeTemplate :: Tree Text -> Html ()
treeTemplate tree = html_ $ do
  head_ $ do
    title_ "Endi's Wiki - Tree"
    link_ [rel_ "stylesheet", href_ "./style.css"]
  body_ $ do
    div_ [class_ "container"] $ do
      div_ [class_ "tree"] $ do
        ul_ $ do
          treeToHtml tree


treeToHtml :: Tree Text -> Html ()
treeToHtml (Node x []) = do
  li_ $ a_ [href_ (x `append` ".html")] $ toHtml x
treeToHtml (Node x ts) = do
  li_ $ a_ [href_ (x `append` ".html")] $ toHtml x
  ul_ $ subtreesToHtml ts
 where
  subtreesToHtml []       = ""
  subtreesToHtml (t : ts) = do
    treeToHtml t
    subtreesToHtml ts

buildWiki :: IO ()
buildWiki = do
  markdownFiles <- getMarkdownFiles =<< getCurrentDirectory
  notes         <- buildNotes markdownFiles
  tree          <- buildTree notes
  writeNotesToRes notes
  writeTreeToRes tree

writeTreeToRes :: Tree Text -> IO ()
writeTreeToRes tree = do
  currentDir <- getCurrentDirectory
  DTIO.writeFile (currentDir ++ "/.res/" ++ "tree.html")
                 (toStrict $ renderText $ treeTemplate tree)

writeNotesToRes :: [Note] -> IO ()
writeNotesToRes notes = do
  currentDir  <- getCurrentDirectory
  dirContents <- listDirectory currentDir
  if ".res" `elem` dirContents
    then do
      removeDirectoryRecursive (currentDir ++ "/.res")
      createDirectory (currentDir ++ "/.res")
    else createDirectory (currentDir ++ "/.res")
  mapM_
    (\n -> DTIO.writeFile
      (currentDir ++ "/.res/" ++ unpack (Builder.Note.title n) ++ ".html")
      (toStrict $ renderText $ Builder.Wiki.template n)
    )
    notes
  DTIO.writeFile (currentDir ++ "/.res/" ++ "style.css")
                 (toStrict $ render styling)

getMarkdownFiles :: FilePath -> IO [FilePath]
getMarkdownFiles currentDir = Prelude.filter isMarkdownFile
  <$> listDirectory currentDir
 where
  isMarkdownFile :: FilePath -> Bool
  isMarkdownFile fileName = take 3 (Prelude.reverse fileName) == "dm."
