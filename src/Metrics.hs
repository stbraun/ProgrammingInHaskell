{-# LANGUAGE RecordWildCards #-}

module Metrics where

import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import Data.IORef
import Data.Time.Clock (diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)


data AppMetrics = AppMetrics
    { successCount :: Int
    , failureCount :: Int
    , callDuration :: Map.Map String Int
    } deriving (Eq, Show)

newtype Metrics = Metrics { appMetricsStore :: IORef AppMetrics }

newMetrics :: IO Metrics
newMetrics =
  let
    emptyAppMetrics = AppMetrics
      { successCount = 0
      , failureCount = 0
      , callDuration = Map.empty
      }
  in Metrics <$> newIORef emptyAppMetrics


tickSuccess :: Metrics -> IO ()
tickSuccess (Metrics metricsRef) =
    modifyIORef metricsRef $ \m -> m { successCount = 1 + successCount m }


tickFailure :: Metrics -> IO ()
tickFailure (Metrics metricsRef) =
    modifyIORef metricsRef $ \m -> m { failureCount = 1 + failureCount m }


timeFunction ::  Metrics -> String -> IO a -> IO a
timeFunction (Metrics metrics) actionName action = do
    startTine <- getCurrentTime
    result    <- action
    endTime   <- getCurrentTime
    modifyIORef metrics $ \oldMetrics ->
      let
        oldDurationValue = fromMaybe 0 $ Map.lookup actionName (callDuration oldMetrics)
        runDuration = floor . nominalDiffTimeToSeconds $ diffUTCTime endTime startTine
        newDurationValue = oldDurationValue + runDuration
      in oldMetrics {
          callDuration = Map.insert actionName newDurationValue $ callDuration oldMetrics
      }
    pure result


displayMetrics :: Metrics -> IO ()
displayMetrics (Metrics metrics) =
    pure metrics >>= readIORef >>= print

