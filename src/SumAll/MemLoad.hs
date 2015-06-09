{-# LANGUAGE NamedFieldPuns #-}
{- |
  This module provides utilities to tell when a program is overloaded,
  where load is a function of memory. This module is not portable and
  requires GHC because it makes use of internal GHC modules.
-}
module SumAll.MemLoad (
  initMemload,
  MemLoad,
  isOverloaded,
  lastOverloaded
) where


import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, newTVar, writeTVar, readTVar)
import Control.Monad (when, void)
import Data.Int (Int64)
import Data.Time.Clock (UTCTime, getCurrentTime)
import GHC.Stats (GCStats(GCStats, currentBytesUsed), getGCStats,
  getGCStatsEnabled)


{- |
  This data type provides a handle on the memory load system.
-}
data MemLoad = MemLoad {
    lastOverloadedT :: TVar (Maybe UTCTime),
    isOverloadedT :: TVar Bool,
    maxMem :: Int64
  }

{- |
  Initialize the memory load system and return a handle that can be
  used to access load information. It is really only valuable to do
  this once your whole program. If GHC's runtime garbage collection stats are
  not enabled, then this IO action will return `Nothing`.
-}
initMemload
  :: Int64
    -- ^ @maxMem@ - The amount of memory we need to consume before we
    --              decide we are overloaded.
  -> IO (Maybe MemLoad)
    -- ^ Returns Nothing if GC stats are not enabled using, e.g., +RTS -T -RTS.
initMemload maxMem = do
    enabled <- getGCStatsEnabled
    if enabled
      then do
        lastOverloadedT <- atomically (newTVar Nothing)
        isOverloadedT <- atomically (newTVar False)
        let memload = MemLoad {lastOverloadedT, isOverloadedT, maxMem}
        (void . forkIO) (checkPeriodically memload)
        return (Just memload)
      else return Nothing
  where
    checkPeriodically memload@MemLoad {
          lastOverloadedT,
          isOverloadedT
        }
      = do
        GCStats {currentBytesUsed} <- getGCStats
        now <- getCurrentTime
        let overloaded = currentBytesUsed >= maxMem
        atomically $ do
          writeTVar isOverloadedT overloaded
          when overloaded (writeTVar lastOverloadedT (Just now))
        threadDelay oneSecond
        checkPeriodically memload

    oneSecond = 1000000


{- |
  Figure out if we are currently overloaded.
-}
isOverloaded
  :: MemLoad
    -- ^ The handle on the memory load management system.
  -> IO Bool
isOverloaded MemLoad {isOverloadedT} =
  atomically (readTVar isOverloadedT)


{- |
  Figure out when the last time we were overloaded was. Returns `Nothing` if
  there never was such a time.
-}
lastOverloaded
  :: MemLoad
    -- ^ The handle on the memory load management system.
  -> IO (Maybe UTCTime)
lastOverloaded MemLoad {lastOverloadedT} =
  atomically (readTVar lastOverloadedT)


