{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, MultiParamTypeClasses, UndecidableInstances, AllowAmbiguousTypes, ScopedTypeVariables, FunctionalDependencies, FlexibleContexts, ConstraintKinds #-}
-- | This module contains implementation of @HasLogger@, @HasLogBackend@, @HasLogContext@ instances for IO monad.
-- This implementation uses thread-local storage, so each thread will have it's own logging state (context and logger).
--
-- This module is not automatically re-exported by System.Log.Heavy, because in many cases it is more convinient to maintain
-- logging state within monadic context, than in global variable.
--
-- Note: implementations of @HasLogger@, @HasLogBackend@, @HasLogContext@ for IO, provided by this module, work only inside
-- @withLoggingIO@ call. If you try to call logging functions outside, you will get runtime error.
--
module System.Log.Heavy.IO
  ( withLoggingIO
  ) where

import Control.Exception
import Data.IORef
import Data.TLS.GHC

import System.IO.Unsafe (unsafePerformIO)

import System.Log.Heavy.Types

-- | Logging state stored in TLS (thread-local storage)
data LoggingIOState = LoggingIOState {
    liosLogger :: SpecializedLogger
  , liosBackend :: AnyLogBackend
  , liosContext :: LogContext
  }

-- | This global variable stores logging state.
-- Nothing inside IORef means that logging state is not initialized yet
-- or is already deinitialized.
loggingTLS :: TLS (IORef (Maybe LoggingIOState))
loggingTLS = unsafePerformIO $ mkTLS $ do
    newIORef Nothing
{-# NOINLINE loggingTLS #-}

-- | Execute IO actions with logging.
--
-- Note 1: logging methods calls in IO monad are only valid inside @withLoggingIO@.
--         If you try to call them outside of this function, you will receive runtime error.
-- 
-- Note 2: if you will for some reason call @withLoggingIO@ inside @withLoggingIO@ within one
--         thread, you will receive runtime error.
-- 
-- Note 3: You can call @withLoggingIO@ syntactically inside @withLoggingIO@, but within other
--         thread. I.e., the construct like following is valid:
--
--         @
--         withLoggingIO settings $ do
--             \$info "message" ()
--             ...
--             forkIO $ do
--                 withLoggingIO settings $ do
--                     \$info "message" ()
--                     ...
--         @
--
withLoggingIO :: LoggingSettings -- ^ Settings of arbitrary logging backend
              -> IO a            -- ^ Actions to be executed with logging
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

-- | Get current logging state. Fail if it is not initialized yet.
getLogginngIOState :: IO LoggingIOState
getLogginngIOState = do
  ioref <- getTLS loggingTLS
  mbState <- readIORef ioref
  case mbState of
    Nothing -> fail "get: Logging IO state is not initialized. See withLoggingIO."
    Just st -> return st

-- | Modify logging state with pure function.
-- Fail if logging state is not initialized yet.
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

