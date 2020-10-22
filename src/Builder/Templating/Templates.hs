{-# LANGUAGE OverloadedStrings #-}
module Builder.Templating.Templates
  ( noteTemplate
  , treeTemplate
  )
where

import           Lucid.Base
import           Lucid.Html5

import           Builder.Note
import           Data.Tree
import           Data.Text

noteTemplate :: Note -> Html ()
noteTemplate note = html_ $ do
  head_ $ do
    title_ "Endi's Wiki"
    link_ [rel_ "stylesheet", href_ "./style.css"]
  body_ $ do
    div_ [class_ "container"] $ do
      div_ [class_ "note"] $ do
        toHtmlRaw $ Builder.Note.html note

treeTemplate :: Tree Text -> [Text] -> Html ()
treeTemplate tree orphans = html_ $ do
  head_ $ do
    title_ "Endi's Wiki - Tree"
    link_ [rel_ "stylesheet", href_ "./style.css"]
  body_ $ do
    div_ [class_ "container"] $ do
      h3_ "Tree"
      div_ [class_ "tree"] $ do
        ul_ $ do
          treeToHtml tree

      div_ [class_ "orphans"] $ do
        h3_ "Orphans"
        ul_ $ do
          mapM_ (\o -> li_ $ a_ [href_ (o `append` ".html")] $ toHtml o) orphans


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
