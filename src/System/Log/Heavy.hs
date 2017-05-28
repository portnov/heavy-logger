{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, MultiParamTypeClasses, UndecidableInstances #-}

module System.Log.Heavy where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Base
import Control.Monad.Logger (MonadLogger (..), LogLevel (..))
import Control.Monad.Trans.Control
import Control.Exception
import Data.String
import Data.Monoid
import Data.List
import Language.Haskell.TH
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Text as T
import System.Log.FastLogger as F
import qualified System.Posix.Syslog as Syslog

import System.Log.Heavy.Types
import System.Log.Heavy.Format
import System.Log.Heavy.Backends

withLogging :: MonadIO m => LogBackend -> (m a -> IO a) -> LoggingT m a -> m a
withLogging (LogBackend b) = withLoggingB b

