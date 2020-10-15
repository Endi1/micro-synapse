module Web.Server
  ( runServer
  )
where

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
                                                )

runServer :: IO ()
runServer = do
  withManager $ \mgr -> do
    watchDir mgr "." (const True) filesChanged
    putStrLn "Running server on port 3000"
    buildWiki
    simpleHTTP Conf { port        = 3000
                    , validator   = Nothing
                    , logAccess   = Just logMAccess
                    , timeout     = 3000
                    , threadGroup = Nothing
                    }
      $ serveDirectory EnableBrowsing ["index.html"] ".res"

filesChanged :: Event -> IO ()
filesChanged _ = buildWiki
