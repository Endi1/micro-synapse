{-# LANGUAGE OverloadedStrings #-}
module Web.Server
  ( runServer
  )
where

import           Data.Text
import           Happstack.Server               ( serveDirectory
                                                , logMAccess
                                                , simpleHTTP
                                                , Browsing(EnableBrowsing)
                                                , Conf
                                                  ( Conf
                                                  , port
                                                  , validator
                                                  , logAccess
                                                  , timeout
                                                  , threadGroup
                                                  )
                                                )
import           Builder.Wiki                   ( buildWiki )
import           System.FSNotify                ( watchDir
                                                , withManager
                                                , Event
                                                , eventPath
                                                )

runServer :: IO ()
runServer = do
  withManager $ \mgr -> do
    watchDir mgr "." predicate filesChanged
    putStrLn "Running server on port 3000"
    buildWiki
    server

server :: IO ()
server =
  simpleHTTP Conf { port        = 3000
                  , validator   = Nothing
                  , logAccess   = Just logMAccess
                  , timeout     = 3000
                  , threadGroup = Nothing
                  }
    $ serveDirectory EnableBrowsing ["index.html"] ".res"

filesChanged :: Event -> IO ()
filesChanged e = do
  print e
  buildWiki

predicate :: Event -> Bool
predicate event = not ("/.res" `isInfixOf` pack (eventPath event))
