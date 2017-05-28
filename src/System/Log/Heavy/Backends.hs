{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, MultiParamTypeClasses, UndecidableInstances #-}

module System.Log.Heavy.Backends
  (
  -- $description
  -- * Backends
  FastLoggerSettings (..),
  SyslogSettings (..),
  -- * Default settings
  defStdoutSettings,
  defStderrSettings,
  defFileSettings,
  defaultSyslogSettings,
  defaultSyslogFormat,
  -- * Utilities for other backends implementation
  checkLogLevel
  ) where

import Control.Monad
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader
import Control.Monad.Logger (MonadLogger (..), LogLevel (..))
import Data.List (isPrefixOf)
import qualified Data.Text as T
import qualified Data.ByteString.Unsafe as BSU
import qualified System.Posix.Syslog as Syslog
import System.Log.FastLogger as F

import System.Log.Heavy.Types
import System.Log.Heavy.Format

-- $description
--
-- This module contains several implementation of logging backend.
-- A backend is some kind of target, where your messages will go.
-- Each backend has its own specific settings.

-- | Settings of fast-logger backend. This mostly reflects settings of fast-logger itself.
data FastLoggerSettings = FastLoggerSettings {
    lsFilter :: LogFilter -- ^ Log messages filter
  , lsFormat :: LogFormat -- ^ Log message format
  , lsType :: F.LogType   -- ^ Fast-logger target settings
  }

-- | Default settings for fast-logger stdout output
defStdoutSettings :: FastLoggerSettings
defStdoutSettings = FastLoggerSettings defaultLogFilter defaultLogFormat (F.LogStdout F.defaultBufSize)

-- | Default settings for fast-logger stderr output
defStderrSettings :: FastLoggerSettings
defStderrSettings = FastLoggerSettings defaultLogFilter defaultLogFormat (F.LogStderr F.defaultBufSize)

-- | Default settings for fast-logger file output.
-- This implies log rotation when log file size reaches 10Mb.
defFileSettings :: FilePath -> FastLoggerSettings
defFileSettings path = FastLoggerSettings defaultLogFilter defaultLogFormat (F.LogFile spec F.defaultBufSize)
  where spec = F.FileLogSpec path (10*1024*1024) 3

instance IsLogBackend FastLoggerSettings where
    -- withLogging :: (MonadIO m) => FastLoggerSettings -> (m a -> IO a) -> LoggingT m a -> m a
    withLoggingB settings runner (LoggingT actions) = do
        liftIO $ do
          tcache <- newTimeCache simpleTimeFormat'
          withTimedFastLogger tcache (lsType settings) $ \logger ->
            runner $ runReaderT actions $ mkLogger logger settings
      where
        mkLogger :: TimedFastLogger -> FastLoggerSettings -> Logger
        mkLogger logger s m = do
          let fltr = lsFilter s
          let format = lsFormat s
          when (checkLogLevel fltr m) $ do
            logger $ formatLogMessage format m

-- | Settings for syslog backend. This mostly reflects syslog API.
data SyslogSettings = SyslogSettings {
    ssFilter :: LogFilter         -- ^ Log messages filter
  , ssFormat :: LogFormat         -- ^ Log message format. Usually you do not want to put time here,
                                  --   because syslog writes time to log by itself by default.
  , ssIdent :: String             -- ^ Syslog source identifier. Usually the name of your program.
  , ssOptions :: [Syslog.Option]  -- ^ Syslog options
  , ssFacility :: Syslog.Facility -- ^ Syslog facility. It is usally User, if you are writing user-space
                                  --   program.
  }

-- | Default settings for syslog backend
defaultSyslogSettings :: SyslogSettings
defaultSyslogSettings = SyslogSettings defaultLogFilter defaultSyslogFormat "application" [] Syslog.User

-- | Default log message format fof syslog backend:
-- @[$level] $source: $message@
defaultSyslogFormat :: LogFormat
defaultSyslogFormat = "[$level] $source: $message"

instance IsLogBackend SyslogSettings where
    withLoggingB settings runner (LoggingT actions) = do
        liftIO $ do
          tcache <- newTimeCache simpleTimeFormat'
          Syslog.withSyslog (ssIdent settings) (ssOptions settings) (ssFacility settings) $ do
            let logger = mkSyslogLogger tcache settings
            runner $ runReaderT actions logger
      where
        mkSyslogLogger :: IO FormattedTime -> SyslogSettings -> Logger
        mkSyslogLogger tcache s m = do
            let fltr = ssFilter s
                format = ssFormat s
                facility = ssFacility s
            when (checkLogLevel fltr m) $ do
              time <- tcache
              let str = formatLogMessage format m time
              BSU.unsafeUseAsCStringLen (fromLogStr str) $
                  Syslog.syslog (Just facility) (levelToPriority $ lmLevel m)

        levelToPriority :: LogLevel -> Syslog.Priority
        levelToPriority LevelDebug = Syslog.Debug
        levelToPriority LevelInfo  = Syslog.Info
        levelToPriority LevelWarn  = Syslog.Warning
        levelToPriority LevelError = Syslog.Error
        levelToPriority (LevelOther level) =
            case level of
                "Emergency" -> Syslog.Emergency
                "Alert"     -> Syslog.Alert
                "Critical"  -> Syslog.Critical
                "Notice"    -> Syslog.Notice
                _ -> error $ "unknown log level: " ++ T.unpack level

-- | Check if message level matches given filter.
checkLogLevel :: LogFilter -> LogMessage -> Bool
checkLogLevel fltr m =
    case lookup (bestMatch (lmSource m) (map fst fltr)) fltr of
      Nothing -> False
      Just level -> lmLevel m >= level
  where
    bestMatch :: LogSource -> [LogSource] -> LogSource
    bestMatch src list = go [] src list

    go best src [] = best
    go best src (x:xs)
      | src == x = x
      | x `isPrefixOf` src && length x > length best = go x src xs
      | otherwise = go best src xs

