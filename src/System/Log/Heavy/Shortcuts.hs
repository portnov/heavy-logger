{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, MultiParamTypeClasses, UndecidableInstances #-}

-- | This module contains some shortcut functions that can be of use in simple usage cases.
module System.Log.Heavy.Shortcuts
  (
    -- * Log a message
    reportError, warning, info, debug,
    -- * Creating a message
    errorMessage, warnMessage, infoMessage, debugMessage
  ) where

import Control.Monad.Trans
import Control.Monad.Logger (LogLevel (..))
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Format.Heavy as F

import System.Log.Heavy.Types
import System.Log.Heavy.Backends

-- | Message stub with Info severity.
infoMessage :: F.VarContainer vars => TL.Text -> vars -> LogMessage
infoMessage fmt vars = LogMessage LevelInfo [] undefined fmt vars

-- | Message stub with Debug severity.
debugMessage :: F.VarContainer vars => TL.Text -> vars -> LogMessage
debugMessage fmt vars = LogMessage LevelDebug [] undefined fmt vars

-- | Message stub with Error severity.
errorMessage :: F.VarContainer vars => TL.Text -> vars -> LogMessage
errorMessage fmt vars = LogMessage LevelError [] undefined fmt vars

-- | Message stub with Warning severity.
warnMessage :: F.VarContainer vars => TL.Text -> vars -> LogMessage
warnMessage fmt vars = LogMessage LevelWarn [] undefined fmt vars

-- | Log debug message.
-- Note: this message will not contain source information.
debug :: (F.VarContainer vars, MonadIO m) => TL.Text -> vars -> LoggingT m ()
debug fmt vars = logMessage $ debugMessage fmt vars

-- | Log info message.
-- Note: this message will not contain source information.
info :: (F.VarContainer vars, MonadIO m) => TL.Text -> vars -> LoggingT m ()
info fmt vars = logMessage $ infoMessage fmt vars

-- | Log error message.
-- Note: this message will not contain source information.
reportError :: (F.VarContainer vars, MonadIO m) => TL.Text -> vars -> LoggingT m ()
reportError fmt vars = logMessage $ errorMessage fmt vars

-- | Log warning message.
-- Note: this message will not contain source information.
warning :: (F.VarContainer vars, MonadIO m) => TL.Text -> vars -> LoggingT m ()
warning fmt vars = logMessage $ warnMessage fmt vars
