{-# LANGUAGE OverloadedStrings #-}
module Builder.Tree
  ( buildTree
  , getOrphans
  )
where

import           Data.Text                      ( unpack
                                                , Text
                                                )
import           Builder.Note                   ( Note(title, raw)
                                                , buildNote
                                                )
import           Data.Tree
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

getOrphans :: Tree Text -> [Note] -> [Text]
getOrphans tree allNotes =
  let allNodes      = flatten tree
      allNoteTitles = [ title n | n <- allNotes ]
  in  filter (`notElem` allNodes) allNoteTitles
