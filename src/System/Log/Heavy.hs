{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, MultiParamTypeClasses, UndecidableInstances #-}

module System.Log.Heavy
  (
    -- * Reexports
    module System.Log.Heavy.Types,
    module System.Log.Heavy.Format,
    module System.Log.Heavy.Backends,
    withLogging,
    -- * Some shortcuts
    errorMessage, infoMessage, debugMessage
  ) where

import Control.Monad.Trans
import Control.Monad.Logger (LogLevel (..))
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Format.Heavy as F

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

infoMessage :: F.VarContainer vars => TL.Text -> vars -> LogMessage
infoMessage fmt vars = LogMessage LevelInfo [] undefined fmt vars

debugMessage :: F.VarContainer vars => TL.Text -> vars -> LogMessage
debugMessage fmt vars = LogMessage LevelDebug [] undefined fmt vars

errorMessage :: F.VarContainer vars => TL.Text -> vars -> LogMessage
errorMessage fmt vars = LogMessage LevelError [] undefined fmt vars

