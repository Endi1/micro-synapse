module Builder.Types
  ( NoteHTML
  , NoteAndHTML
  )
where

import           Data.Text
import           Builder.Note


type NoteHTML = Text
type NoteAndHTML = (Note, NoteHTML)
