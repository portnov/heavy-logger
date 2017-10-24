{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, MultiParamTypeClasses, UndecidableInstances, AllowAmbiguousTypes, ScopedTypeVariables, FunctionalDependencies, FlexibleContexts, ConstraintKinds #-}
-- | This module contains default implementation of @HasLogBackend@,
-- @HasLogContext@, @HasLogger@ instances, based on @ReaderT@ - @LoggingT@
-- monad transformer.
module System.Log.Heavy.LoggingT
  (
    LoggingT (LoggingT), LoggingTState (..),
    runLoggingT
  ) where

import Control.Monad.Reader
import Control.Monad.Base
import Control.Monad.Trans.Control

import System.Log.Heavy.Types
import System.Log.Heavy.Level

-- | State of @LoggingT@ monad
data LoggingTState = LoggingTState {
    ltsLogger :: SpecializedLogger
  , ltsBackend :: AnyLogBackend
  , ltsContext :: LogContext
  }

-- | Logging monad transformer.
-- This is just a default implementation of @HasLogging@ interface.
-- Applications are free to use this or another implementation.
newtype LoggingT m a = LoggingT {
    runLoggingT_ :: ReaderT LoggingTState m a
  }
  deriving (Functor, Applicative, Monad, MonadReader LoggingTState, MonadTrans)

deriving instance MonadIO m => MonadIO (LoggingT m)

instance MonadIO m => MonadBase IO (LoggingT m) where
  liftBase = liftIO

instance MonadTransControl LoggingT where
    type StT LoggingT a = StT (ReaderT LoggingTState) a
    liftWith = defaultLiftWith LoggingT runLoggingT_
    restoreT = defaultRestoreT LoggingT

instance (MonadBaseControl IO m, MonadIO m) => MonadBaseControl IO (LoggingT m) where
    type StM (LoggingT m) a = ComposeSt LoggingT m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM

instance Monad m => HasLogger (LoggingT m) where
  getLogger = asks ltsLogger
  localLogger l actions = LoggingT $ ReaderT $ \lts -> runReaderT (runLoggingT_ actions) $ lts {ltsLogger = l}

instance (Monad m) => HasLogContext (LoggingT m) where
  getLogContext = asks ltsContext

  withLogContext frame actions =
    LoggingT $ ReaderT $ \lts -> runReaderT (runLoggingT_ actions) $ lts {ltsContext = frame: ltsContext lts}

-- | Run logging monad
runLoggingT :: LoggingT m a -> LoggingTState -> m a
runLoggingT actions context = runReaderT (runLoggingT_ actions) context

instance Monad m => HasLogBackend AnyLogBackend (LoggingT m) where
  getLogBackend = asks ltsBackend

