{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, MultiParamTypeClasses, UndecidableInstances #-}

module System.Log.Heavy where

import Control.Monad.Trans

import System.Log.Heavy.Types
import System.Log.Heavy.Format
import System.Log.Heavy.Backends

withLogging :: MonadIO m => LogBackend -> (m a -> IO a) -> LoggingT m a -> m a
withLogging (LogBackend b) = withLoggingB b

