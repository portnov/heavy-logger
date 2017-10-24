{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, MultiParamTypeClasses, UndecidableInstances, AllowAmbiguousTypes, ScopedTypeVariables, FunctionalDependencies, FlexibleContexts, ConstraintKinds #-}

module System.Log.Heavy.IO
  ( withLoggingIO
  ) where

import Control.Exception
import Data.IORef
import Data.TLS.GHC

import System.IO.Unsafe (unsafePerformIO)

import System.Log.Heavy.Types

data LoggingIOState = LoggingIOState {
    liosLogger :: SpecializedLogger
  , liosBackend :: AnyLogBackend
  , liosContext :: LogContext
  }

loggingTLS :: TLS (IORef (Maybe LoggingIOState))
loggingTLS = unsafePerformIO $ mkTLS $ do
    newIORef Nothing
{-# NOINLINE loggingTLS #-}

withLoggingIO :: LoggingSettings
              -> IO a
              -> IO a
withLoggingIO (LoggingSettings settings) actions =
    bracket (init settings)
            (cleanup)
            (\tls -> withBackend tls actions)
  where
    init settings = do
      ioref <- getTLS loggingTLS
      mbState <- readIORef ioref
      case mbState of
        Just _ -> fail "Logging IO state is already initialized. withLoggingIO was called twice?"
        Nothing -> do
              backend <- initLogBackend settings
              let logger = makeLogger backend
              let st = LoggingIOState logger (AnyLogBackend backend) []
              writeIORef ioref (Just st)
              return st

    cleanup st = do
      ioref <- getTLS loggingTLS
      freeAllTLS loggingTLS
      writeIORef ioref Nothing

    withBackend st actions = actions

getLogginngIOState :: IO LoggingIOState
getLogginngIOState = do
  ioref <- getTLS loggingTLS
  mbState <- readIORef ioref
  case mbState of
    Nothing -> fail "get: Logging IO state is not initialized. See withLoggingIO."
    Just st -> return st

modifyLoggingIOState :: (LoggingIOState -> LoggingIOState) -> IO ()
modifyLoggingIOState fn = do
  ioref <- getTLS loggingTLS
  modifyIORef ioref $ \mbState ->
    case mbState of
      Nothing -> error "modify: Logging IO state is not initialized. See withLoggingIO."
      Just st -> Just (fn st)

instance HasLogBackend AnyLogBackend IO where
  getLogBackend = do
    st <- getLogginngIOState 
    return $ liosBackend st

instance HasLogger IO where
  getLogger = do
    st <- getLogginngIOState 
    return $ liosLogger st

  localLogger logger actions = do
    oldLogger <- getLogger
    modifyLoggingIOState $ \st -> st {liosLogger = logger}
    result <- actions
    modifyLoggingIOState $ \st -> st {liosLogger = oldLogger}
    return result

instance HasLogContext IO where
  getLogContext = do
    st <- getLogginngIOState 
    return $ liosContext st

  withLogContext frame actions = do
    oldContext <- getLogContext
    modifyLoggingIOState $ \st -> st {liosContext = frame:oldContext}
    result <- actions
    modifyLoggingIOState $ \st -> st {liosContext = oldContext}
    return result

