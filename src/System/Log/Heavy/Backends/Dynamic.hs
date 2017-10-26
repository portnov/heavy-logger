{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, MultiParamTypeClasses, UndecidableInstances #-}

module System.Log.Heavy.Backends.Dynamic
  (
    DynamicBackend,
    DynamicBackendHandle,
    newDynamicBackendHandle,
    updateDynamicBackendSettings,
    LogBackendSettings (..)
  ) where 

import Control.Concurrent
import Control.Concurrent.STM

import System.Log.Heavy.Types

data DynamicBackendHandle = DynamicBackendHandle {
    dbhBroadcast :: TChan LoggingSettings
  , dbhDefault :: LoggingSettings
  }

newDynamicBackendHandle :: LoggingSettings -> IO DynamicBackendHandle
newDynamicBackendHandle settings = do
  broadcast <- newBroadcastTChanIO
  return $ DynamicBackendHandle broadcast settings

-- newDynamicSettings :: LoggingSettings -> IO DynamicSettings
-- newDynamicSettings settings = do
--   handle <- newDynamicBackendHandle settings
--   return $ DynamicSettings handle

updateDynamicBackendSettings :: DynamicBackendHandle -> LoggingSettings -> IO ()
updateDynamicBackendSettings handle settings = do
  atomically $ writeTChan (dbhBroadcast handle) settings

-- | Dynamic logging backend allows to change logging backend or it's settings
-- in runtime. When it sees new backend settings, it deinitializes old backend
-- and initializes new one.
--
-- How to use it:
--
-- * Before creating @DynamicSettings@, you have to select some initial
--   @LoggingSettings@ and create @DynamicBackendHandle@ with it.
-- * When you decide that you want to use new backend settings, call
--   @updateDynamicBackendSettings@ on existing @DynamicBackendHandle@.
--   @DynamicBackend@ will use new settings for the next logging function call.
--
data DynamicBackend = DynamicBackend {
    dbCurrentBackend :: MVar AnyLogBackend
  , dbNewSettings :: TChan LoggingSettings
  }

instance IsLogBackend DynamicBackend where
  data LogBackendSettings DynamicBackend = DynamicSettings DynamicBackendHandle

  initLogBackend (DynamicSettings (DynamicBackendHandle broadcast (LoggingSettings dfltSettings))) = do
    mySettingsChan <- atomically $ dupTChan broadcast
    backend <- initLogBackend dfltSettings
    backendVar <- newMVar (AnyLogBackend backend)
    return $ DynamicBackend backendVar mySettingsChan

  cleanupLogBackend (DynamicBackend backendVar _) = do
    backend <- takeMVar backendVar
    cleanupLogBackend backend

  wouldWriteMessage (DynamicBackend backendVar _) msg = do
    backend <- readMVar backendVar
    wouldWriteMessage backend msg

  makeLogger (DynamicBackend backendVar settingsChan) msg = do
    mbNewSettings <- atomically $ tryReadTChan settingsChan
    case mbNewSettings of
      Nothing -> do
          backend <- readMVar backendVar
          makeLogger backend msg
      Just (LoggingSettings newSettings) -> do
          oldBackend <- takeMVar backendVar
          cleanupLogBackend oldBackend
          newBackend <- initLogBackend newSettings
          putMVar backendVar (AnyLogBackend newBackend)
          makeLogger newBackend msg

