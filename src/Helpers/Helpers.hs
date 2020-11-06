module Helpers.Helpers
  ( readNoteRaw
  )
where

import qualified Data.Text.IO                  as DTIO
                                                ( readFile )
import           System.Directory               ( getCurrentDirectory )
import           Data.Text                      ( Text )

readNoteRaw :: FilePath -> IO Text
readNoteRaw notePath = do
  currentDir <- getCurrentDirectory
  DTIO.readFile (currentDir ++ "/" ++ notePath)
