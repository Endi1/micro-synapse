{-# LANGUAGE OverloadedStrings #-}
module Builder.Tree
  ( buildTree
  )
where

import           Data.Text                      ( unpack
                                                , Text
                                                )
import           Builder.Note                   ( Note(title, raw)
                                                , buildNote
                                                )
import           Data.Tree                      ( Tree
                                                , unfoldTreeM
                                                )
import           Parser.Innerlinks              ( parseInnerLinks )

buildTree :: [Note] -> IO (Tree Text)
buildTree allNotes =
  let rootNote = head $ filter (\n -> title n == "index") allNotes
  in  unfoldTreeM buildChildrenFromNode $ title rootNote

buildChildrenFromNode :: Text -> IO (Text, [Text])
buildChildrenFromNode currentNoteTitle = do
  currentNote <- buildNote (unpack currentNoteTitle ++ ".md")
  let currentNoteChildrenTitles = parseInnerLinks $ raw currentNote
  return (currentNoteTitle, currentNoteChildrenTitles)
