{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Control.Concurrent.MVar        (MVar, modifyMVar_, newMVar,
                                                 readMVar)
import           Data.Aeson
import           Data.Aeson.TH                  (deriveJSON)
import           Data.Text                      (Text)
import           Data.Time                      (UTCTime, getCurrentTime)
import           Data.Word                      (Word64)
import           GHC.Stats                      (allocated_bytes, getRTSStats,
                                                 getRTSStatsEnabled,
                                                 max_mem_in_use_bytes)
import           Network.HTTP.Types             (status200)
import           Network.Wai                    (Application, pathInfo,
                                                 responseLBS)
import           Network.Wai.Handler.Warp       (run)
import           Network.Wai.Handler.WebSockets
import           Network.WebSockets             (DataMessage (..), ServerApp,
                                                 acceptRequest,
                                                 defaultConnectionOptions,
                                                 sendDataMessage)
import           Prelude

data ServerStats =
  ServerStats
    { serverStatsUpSince        :: !UTCTime
    , serverStatsRtsMem         :: !Word64
    , serverStatsAllocatedBytes :: !Word64
    , serverStatsHits           :: !Int
    } deriving (Show, Eq)

$(deriveJSON defaultOptions ''ServerStats)

main :: IO ()
main = do
  now <- getCurrentTime
  let stats =
        ServerStats
          { serverStatsUpSince = now
          , serverStatsRtsMem = 0
          , serverStatsAllocatedBytes = 0
          , serverStatsHits = 0
          }
  serverStats <- newMVar stats
  run 3000 (app serverStats)

app :: MVar ServerStats -> Application
app stats = websocketsOr defaultConnectionOptions (wsApp stats) (backupApp stats)

wsApp :: MVar ServerStats -> ServerApp
wsApp stats pending_conn = do
  rtsStats <- readMVar stats
  conn <- acceptRequest pending_conn
  sendDataMessage conn (Text (encode rtsStats) Nothing :: DataMessage)

backupApp :: MVar ServerStats -> Application
backupApp stats req respond = do
  let path = pathInfo req
  if path `elem` redundantPaths
    then respond $ responseLBS status200 [] "Favicon request"
    else do
      rtsStats <- getRTSStats
      modifyMVar_
        stats
        (\stats' ->
           return $
             stats'
               { serverStatsRtsMem = max_mem_in_use_bytes rtsStats
               , serverStatsAllocatedBytes = allocated_bytes rtsStats
               , serverStatsHits    = serverStatsHits stats' + 1
               }
        )
      respond $ responseLBS status200 [] "Welcome to Hasura"

redundantPaths :: [[Text]]
redundantPaths = [["favicon.ico"]]
