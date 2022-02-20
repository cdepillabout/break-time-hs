{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BreakTime where

import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM (TVar, TMVar, atomically, newEmptyTMVarIO, newTMVarIO, newTVarIO, readTVar, retry, takeTMVar, tryPutTMVar, writeTVar)
import Control.Monad (void)

data CountDown = CountDown
  { timeLeft :: !NominalDiffTime
    -- ^ Time left until either a break starts, or a normal countdown starts again.
  , prevTarget :: !UTCTime
    -- ^ The previous time we were trying to sleep until.
  , countDownEndsAt :: !UTCTime
    -- ^ The time this CountDown is supposed to end.
  }
  deriving stock Show

data BreakTimeState
  = BTSWork !CountDown
  | BTSBreak !CountDown
  | BTSPause
  deriving stock Show

data BreakTimeEnv = BreakTimeEnv
  { state :: !(TVar BreakTimeState)
  , workTime :: !NominalDiffTime
    -- ^ The amount of time to run a normal CountDown (the time between breaks).
  , breakTime :: !NominalDiffTime
    -- ^ The amount of time to break.
  , doReset :: TMVar ()
  , isPaused :: TVar Bool
  }

showBreakTimeEnv :: BreakTimeEnv -> IO String
showBreakTimeEnv BreakTimeEnv { state, workTime, breakTime } = do
  st <- atomically $ readTVar state
  pure $
    "BreakTimeEnv: (break every " <> show workTime <>
    " seconds, breaks last for " <> show breakTime <>
    " seconds): " <> show st

newCountDown :: UTCTime -> NominalDiffTime -> CountDown
newCountDown currTime countDownTime =
  CountDown
    { timeLeft = countDownTime
    , prevTarget = currTime
    , countDownEndsAt = addUTCTime countDownTime currTime
    }

newBreakTimeEnv :: IO BreakTimeEnv
newBreakTimeEnv = do
  currTime <- getCurrentTime
  doReset <- newEmptyTMVarIO
  isPaused <- newTVarIO False
  let workTime = 15
      breakTime = 10
      countDown = newCountDown currTime workTime
  state <- newTVarIO $ BTSWork countDown
  pure BreakTimeEnv {..}

nominalDiffTimeToMicroSecs :: NominalDiffTime -> Int
nominalDiffTimeToMicroSecs diffTime =
  let diffSeconds = fromRational $ toRational diffTime :: Double
  in round (diffSeconds * 1_000_000)

nominalDiffThreadDelay :: NominalDiffTime -> IO ()
nominalDiffThreadDelay = threadDelay . nominalDiffTimeToMicroSecs

waitForReset :: TMVar () -> IO ()
waitForReset resetTMVar = atomically (takeTMVar resetTMVar)

reset :: BreakTimeEnv -> IO ()
reset BreakTimeEnv { doReset } = void $ atomically (tryPutTMVar doReset ())

pause :: BreakTimeEnv -> IO ()
pause BreakTimeEnv { isPaused }= atomically (writeTVar isPaused True)

unpause :: BreakTimeEnv -> IO ()
unpause BreakTimeEnv { isPaused }= atomically (writeTVar isPaused False)

waitForPause :: TVar Bool -> IO ()
waitForPause isPaused =
  atomically $ do
    isPaused' <- readTVar isPaused
    if isPaused'
      then pure ()
      else retry

waitForUnpause :: TVar Bool -> IO ()
waitForUnpause isPaused =
  atomically $ do
    isPaused' <- readTVar isPaused
    if isPaused'
      then retry
      else pure ()

runBreakTime :: BreakTimeEnv -> IO ()
runBreakTime BreakTimeEnv { state, workTime, breakTime, doReset, isPaused } = go
  where
    go :: IO ()
    go = do
      breakTimeState <- atomically $ readTVar state
      case breakTimeState of
        BTSPause -> do
          waitForUnpause isPaused
          timeAfterPause <- getCurrentTime
          let countDown = newCountDown timeAfterPause workTime
          atomically $ writeTVar state $! BTSWork countDown
          go
        BTSWork CountDown { timeLeft, prevTarget, countDownEndsAt } -> do
          let !aimingTime = addUTCTime 1 prevTarget
          currTime <- getCurrentTime
          let diffTime = diffUTCTime aimingTime currTime
          raceRes <-
            race
              (nominalDiffThreadDelay diffTime)
              (race
                (waitForReset doReset)
                (waitForPause isPaused)
              )
          timeAfterSleep <- getCurrentTime
          case raceRes of
            -- the normal sleep finished
            Left () -> do
              let !newTimeLeft = diffUTCTime countDownEndsAt timeAfterSleep
              if countDownEndsAt < timeAfterSleep
                then do
                  let countDown = newCountDown timeAfterSleep breakTime
                  atomically $ writeTVar state $! BTSBreak countDown
                  go
                else do
                  putStrLn $ "still have " <> show newTimeLeft <> " seconds left to work"
                  atomically $ writeTVar state $!
                    BTSWork CountDown
                      { timeLeft = newTimeLeft
                      , prevTarget = aimingTime
                      , countDownEndsAt
                      }
                  go
            -- there was a reset event
            Right (Left ()) -> do
              putStrLn "found reset event"
              let countDown = newCountDown timeAfterSleep workTime
              atomically $ writeTVar state $! BTSWork countDown
              go
            Right (Right ()) -> do
              putStrLn "now paused"
              atomically $ writeTVar state $! BTSPause
              go
        BTSBreak CountDown { timeLeft, prevTarget, countDownEndsAt } -> do
          let !aimingTime = addUTCTime 1 prevTarget
          currTime <- getCurrentTime
          let diffTime = diffUTCTime aimingTime currTime
          nominalDiffThreadDelay diffTime
          timeAfterSleep <- getCurrentTime
          let !newTimeLeft = diffUTCTime countDownEndsAt timeAfterSleep
          if countDownEndsAt < timeAfterSleep
            then do
              let countDown = newCountDown timeAfterSleep workTime
              atomically $ writeTVar state $! BTSWork countDown
              go
            else do
              putStrLn $ "still have " <> show newTimeLeft <> " seconds left to break"
              atomically $ writeTVar state $!
                BTSBreak CountDown
                  { timeLeft = newTimeLeft
                  , prevTarget = aimingTime
                  , countDownEndsAt
                  }
              go
