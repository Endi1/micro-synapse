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
import           Data.Text                      ( unpack )
import           Data.Text.Lazy                 ( toStrict )
import           Builder.Note                   ( Note(..)
                                                , buildNotes
                                                )
import           Lucid.Base
import           Lucid.Html5

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
