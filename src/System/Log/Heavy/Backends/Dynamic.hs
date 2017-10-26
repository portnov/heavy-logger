{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, MultiParamTypeClasses, UndecidableInstances #-}
-- | This module incldues logging backend combinators that allow to change
-- underlying backend or it's settings in runtime.
module System.Log.Heavy.Backends.Dynamic
  (
    -- * Dynamic backend
    DynamicBackend,
    DynamicBackendHandle,
    newDynamicBackendHandle,
    updateDynamicBackendSettings,
    LogBackendSettings (..),
    -- * Dynamic filtering
    FilteringM, filteringM, excludingM
  ) where 

import Control.Monad (when)
import Control.Concurrent
import Control.Concurrent.STM

import System.Log.Heavy.Types
import System.Log.Heavy.Util

-- | Abstract handle, that is used to control @DynamicBackend@.
data DynamicBackendHandle = DynamicBackendHandle {
    dbhBroadcast :: TChan LoggingSettings -- ^ Broadcast TChan. It is write-only.
  , dbhDefault :: LoggingSettings         -- ^ Initial logging settings. Used only before first update in TChan.
                                          --   We cannot put this into TChan at beginning, because after dupTChan
                                          --   newly created TChan's will be empty anyway.
  }
-- I put these data into special Handle type, to hide the implementation detail
-- that it uses TChan inside. This detail can be changed in later versions.

-- | Create an instance of @DynamicBackendHandle@.
newDynamicBackendHandle :: LoggingSettings         -- ^ Initial logging settings. This can be changed later with @updateDynamicBackendSettings@.
                     -> IO DynamicBackendHandle
newDynamicBackendHandle settings = do
  broadcast <- newBroadcastTChanIO
  return $ DynamicBackendHandle broadcast settings

-- | Update settings of @DynamicBackend@, which was created by provided handle.
updateDynamicBackendSettings :: DynamicBackendHandle     -- ^ Handle of @DynamicBackend@.
                             -> LoggingSettings          -- ^ New logging settings.
                             -> IO ()
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
-- * It is responsibility of caller code to do not change backends too frequently;
--   for example, if you are checking your config file for updates of logging
--   settings each 10s, you have to check that settings actually changed since
--   last time.
-- 
-- It is possible to create one instance of @DynamicBackendHandle@ and pass it
-- to multiple threads.
--
data DynamicBackend = DynamicBackend {
    dbCurrentBackend :: MVar AnyLogBackend -- ^ Currently used logging backend
  , dbNewSettings :: TChan LoggingSettings -- ^ TChan with updates of settings. This is read only.
  }

-- Implementation is based on idea that Backend instances theirself are thread-safe;
-- i.e. it is safe to create several instances of any IsLogBackend data type in several threads
-- and they will coexist without problems (they do not use any shared state).
-- So, we can have one TChan, in which we can push updated settings when we want;
-- Each instance of DynamicBackend will have its own instance of underlying backend.
instance IsLogBackend DynamicBackend where
  data LogBackendSettings DynamicBackend = DynamicSettings DynamicBackendHandle

  initLogBackend (DynamicSettings (DynamicBackendHandle broadcast (LoggingSettings dfltSettings))) = do
    -- Duplicate broadcast TChan - create "own copy".
    mySettingsChan <- atomically $ dupTChan broadcast
    -- Initialize default backend
    backend <- initLogBackend dfltSettings
    backendVar <- newMVar (AnyLogBackend backend)
    return $ DynamicBackend backendVar mySettingsChan

  cleanupLogBackend (DynamicBackend backendVar _) = do
    -- Cleanup currently used backend
    backend <- takeMVar backendVar
    cleanupLogBackend backend

  wouldWriteMessage (DynamicBackend backendVar _) msg = do
    -- Delegate the call to currently used backend.
    backend <- readMVar backendVar
    wouldWriteMessage backend msg

  makeLogger (DynamicBackend backendVar settingsChan) msg = do
    -- See if there is an update of settings queued.
    mbNewSettings <- atomically $ tryReadTChan settingsChan
    -- Note: tryReadTChan will remove element from channel, if there is any.
    -- But, it will affect only "our own copy" of TChan. If there are other
    -- instances of DynamicBackend living in other threads with the same
    -- Handle, they will still see settings update in their own copies of
    -- TChan.
    case mbNewSettings of
      Nothing -> do
          -- No updates. Use current backend.
          backend <- readMVar backendVar
          makeLogger backend msg
      Just (LoggingSettings newSettings) -> do
          -- New settings came. We have to switch backends.
          oldBackend <- takeMVar backendVar
          -- Cleanup old backend
          cleanupLogBackend oldBackend
          -- Initialize new backend
          newBackend <- initLogBackend newSettings
          putMVar backendVar (AnyLogBackend newBackend)
          -- Delegate call to new backend
          makeLogger newBackend msg

-- | Filtering backend with mutable filter.
data FilteringM b = FilteringBackendM (MVar (LogMessage -> Bool)) b

-- | Specify filter as @LogFilter@. This filter can be changed later.
filteringM :: IsLogBackend b => LogFilter -> LogBackendSettings b -> IO (LogBackendSettings (FilteringM b))
filteringM fltr b = do
  fltrVar <- newMVar (checkLogLevel fltr)
  return $ FilteringM fltrVar b

-- | Exclude messages by filter. This filter can be changed later.
excludingM :: IsLogBackend b => LogFilter -> LogBackendSettings b -> IO (LogBackendSettings (FilteringM b))
excludingM fltr b = do
    fltrVar <- newMVar ex
    return $ FilteringM fltrVar b
  where
    ex msg = not $ checkContextFilter' [LogContextFilter Nothing (Just fltr)] (lmSource msg) (lmLevel msg)

instance IsLogBackend b => IsLogBackend (FilteringM b) where
  data LogBackendSettings (FilteringM b) =
      FilteringM (MVar (LogMessage -> Bool)) (LogBackendSettings b)

  wouldWriteMessage (FilteringBackendM fltrVar _) msg = do
    fltr <- readMVar fltrVar
    return $ fltr msg

  makeLogger (FilteringBackendM fltrVar backend) msg = do
    fltr <- readMVar fltrVar
    when (fltr msg) $ do
      makeLogger backend msg

  initLogBackend (FilteringM fltrVar settings) = do
    backend <- initLogBackend settings
    return $ FilteringBackendM fltrVar backend

  cleanupLogBackend (FilteringBackendM _ backend) = cleanupLogBackend backend

