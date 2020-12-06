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

commonHeadElements :: Html ()
commonHeadElements = do
  meta
  link_ [rel_ "preconnect", href_ "https://fonts.gstatic.com"]
  link_
    [ href_
      "https://fonts.googleapis.com/css2?family=Roboto:ital@0;1&display=swap"
    , rel_ "stylesheet"
    ]
  link_ [rel_ "stylesheet", href_ "./style.css"]


meta :: Html ()
meta = do
  meta_ [content_ "text/html;charset=utf-8", httpEquiv_ "Content-Type"]
  meta_ [content_ "utf-8", httpEquiv_ "encoding"]

navBar :: Html ()
navBar = nav_ [class_ "navbar"] $ do
  ul_ $ do
    li_ $ a_ [href_ "/"] "Home"
    li_ $ a_ [href_ "/tree.html"] "Tree"

noteTemplate :: Note -> Text -> Html ()
noteTemplate note noteHTML = doctypehtml_ $ html_ $ do
  head_ $ do
    commonHeadElements
    title_ $ toHtml $ Builder.Note.title note
  body_ $ do
    div_ [class_ "container"] $ do
      navBar
      div_ [class_ "body"] $ do
        div_ [class_ "note"] $ do
          toHtmlRaw noteHTML

treeTemplate :: Tree Text -> [Text] -> Html ()
treeTemplate tree orphans = doctypehtml_ $ html_ $ do
  head_ $ do
    commonHeadElements
    title_ "Endi's Wiki - Tree"
  body_ $ do
    div_ [class_ "container"] $ do
      navBar
      div_ [class_ "body"] $ do
        h3_ "Tree"
        div_ [class_ "tree"] $ do
          ul_ $ do
            treeToHtml tree

        div_ [class_ "orphans"] $ do
          h3_ "Orphans"
          ul_ $ do
            mapM_ (\o -> li_ $ a_ [href_ (o `append` ".html")] $ toHtml o)
                  orphans


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
