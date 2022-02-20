{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVar, newTVarIO, readTVar, stateTVar, writeTVar)
import Control.Exception (SomeException(SomeException), try)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import GHC.Conc (threadStatus)
import Network.HTTP.Types (status201, status202, status404)
import Web.Scotty (html, body, capture, delete, get, param, post, redirect, setHeader, scotty, status, text)

import BreakTime (BreakTimeEnv, newBreakTimeEnv, pause, reset, runBreakTime, showBreakTimeEnv, unpause)

type LText = Text.Lazy.Text

tshow :: Show a => a -> Text
tshow = Text.pack . show

tlshow :: Show a => a -> LText
tlshow = Text.Lazy.pack . show


---------
-- Env --
---------

data BreakTimeThread = BreakTimeThread ThreadId BreakTimeEnv

runBreakTimeThread :: IO BreakTimeThread
runBreakTimeThread = do
  btEnv <- newBreakTimeEnv
  threadId <- forkIO $ runBreakTime btEnv
  pure $ BreakTimeThread threadId btEnv

data Env = Env
  { threadIdMap :: TVar (HashMap Int BreakTimeThread)
  , nextThreadId :: TVar Int
  }

newEnv :: IO Env
newEnv = do
  threadIdMap <- newTVarIO mempty
  nextThreadId <- newTVarIO 0
  pure $ Env {..}

------------
-- Server --
------------

runServer :: Env -> IO ()
runServer env = do
  scotty 3000 $ do
    post "/break-time" $ do
      breakTimeThread <- liftIO runBreakTimeThread
      let tidMap = threadIdMap env
      let nextTId = nextThreadId env
      newTId <- liftIO $ atomically $ do
        nextTId' <- readTVar nextTId
        modifyTVar' tidMap (HashMap.insert nextTId' breakTimeThread)
        writeTVar nextTId $! (nextTId' + 1)
        pure nextTId'
      status status201
      setHeader "Location" ("/break-time/" <> tlshow newTId)

    get "/break-time/:id" $ do
      (id :: Int) <- param "id"
      tidMap <- liftIO $ atomically $ readTVar (threadIdMap env)
      case HashMap.lookup id tidMap of
        Nothing -> status status404
        Just (BreakTimeThread tid btEnv) -> do
          btEnvStr <- liftIO $ showBreakTimeEnv btEnv
          st <- liftIO $ threadStatus tid
          text $ Text.Lazy.pack btEnvStr <> ", threadStatus: " <> tlshow st <> "\n"

    delete "/break-time/:id" $ do
      (id :: Int) <- param "id"
      tidMap <- liftIO $ atomically $ do
        stateTVar
          (threadIdMap env)
          (\tidMap ->
            let !newTidMap = HashMap.delete id tidMap
            in (tidMap, newTidMap)
          )
      case HashMap.lookup id tidMap of
        Nothing -> status status404
        Just (BreakTimeThread tid _btEnv) -> do
          st <- liftIO $ killThread tid
          status status202

    post "/break-time/:id/reset" $ do
      (id :: Int) <- param "id"
      tidMap <- liftIO $ atomically $ readTVar (threadIdMap env)
      case HashMap.lookup id tidMap of
        Nothing -> status status404
        Just (BreakTimeThread _ btEnv) -> do
          liftIO $ reset btEnv
          status status201

    post "/break-time/:id/pause" $ do
      (id :: Int) <- param "id"
      tidMap <- liftIO $ atomically $ readTVar (threadIdMap env)
      case HashMap.lookup id tidMap of
        Nothing -> status status404
        Just (BreakTimeThread _ btEnv) -> do
          liftIO $ pause btEnv
          status status201

    post "/break-time/:id/unpause" $ do
      (id :: Int) <- param "id"
      tidMap <- liftIO $ atomically $ readTVar (threadIdMap env)
      case HashMap.lookup id tidMap of
        Nothing -> status status404
        Just (BreakTimeThread _ btEnv) -> do
          liftIO $ unpause btEnv
          status status201

defaultMain :: IO ()
defaultMain = do
  env <- newEnv
  e <- try (runServer env)
  case e of
    Left (SomeException ex) -> do
      tidMap <- liftIO $ atomically $ readTVar (threadIdMap env)
      for_ (HashMap.elems tidMap) $ \(BreakTimeThread tid _) -> killThread tid
    Right () -> pure ()
