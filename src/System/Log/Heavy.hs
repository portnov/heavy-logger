{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, MultiParamTypeClasses, UndecidableInstances #-}

module System.Log.Heavy
  (
    -- * Reexports
    module System.Log.Heavy.Types,
    module System.Log.Heavy.Format,
    module System.Log.Heavy.Backends,
    withLogging
  ) where

import Control.Monad.Trans

import System.Log.Heavy.Types
import System.Log.Heavy.Format
import System.Log.Heavy.Backends

-- | Run LoggingT monad within some kind of IO monad.
withLogging :: MonadIO m
            => LogBackend    -- ^ Logging backend settings
            -> (m a -> IO a) -- ^ Runner to run @m@ within @IO@. 
                             --   For example this may be @runReader@ or @evalState@.
                             --   Use @id@ for case when @m@ is @IO@.
            -> LoggingT m a  -- ^ Actions within @LoggingT@ monad.
            -> m a
withLogging (LogBackend b) = withLoggingB b

