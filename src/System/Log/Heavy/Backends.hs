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
import Control.Concurrent
import Data.List (isPrefixOf)
import qualified Data.Text as T
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Text.Format.Heavy as F
import qualified System.Posix.Syslog as Syslog
import System.Log.FastLogger as FL
import Foreign.C.String (CString, newCString)
import Foreign.Marshal.Alloc (free)

import System.Log.Heavy.Types
import System.Log.Heavy.Format

-- $description
--
-- This module contains several implementation of logging backend.
-- A backend is some kind of target, where your messages will go.
-- Each backend has its own specific settings.
--
-- Backends provided are:
--
-- * Fast-logger backend. It allows to write messages to stdout, stderr or arbitrary file.
--
-- * Syslog backend.
--
-- * Chan backend.
--

-- | Settings of fast-logger backend. This mostly reflects settings of fast-logger itself.
data FastLoggerSettings = FastLoggerSettings {
    lsFilter :: LogFilter -- ^ Log messages filter
  , lsFormat :: F.Format -- ^ Log message format
  , lsType :: FL.LogType   -- ^ Fast-logger target settings
  }

-- | Default settings for fast-logger stdout output
defStdoutSettings :: FastLoggerSettings
defStdoutSettings = FastLoggerSettings defaultLogFilter defaultLogFormat (FL.LogStdout FL.defaultBufSize)

-- | Default settings for fast-logger stderr output
defStderrSettings :: FastLoggerSettings
defStderrSettings = FastLoggerSettings defaultLogFilter defaultLogFormat (FL.LogStderr FL.defaultBufSize)

-- | Default settings for fast-logger file output.
-- This implies log rotation when log file size reaches 10Mb.
defFileSettings :: FilePath -> FastLoggerSettings
defFileSettings path = FastLoggerSettings defaultLogFilter defaultLogFormat (FL.LogFile spec FL.defaultBufSize)
  where spec = FL.FileLogSpec path (10*1024*1024) 3

data FastLoggerBackend = FastLoggerBackend {
    flbSettings :: FastLoggerSettings,
    flbTimedLogger :: TimedFastLogger,
    flbCleanup :: IO ()
  }

newFastLoggerBackend :: FastLoggerSettings -> IO FastLoggerBackend
newFastLoggerBackend settings = do
    tcache <- newTimeCache simpleTimeFormat'
    (logger, cleanup) <- newTimedFastLogger tcache (lsType settings)
    return $ FastLoggerBackend settings logger cleanup

cleanupFastLoggerBackend :: FastLoggerBackend -> IO ()
cleanupFastLoggerBackend b = flbCleanup b

instance IsLogBackend FastLoggerBackend where
    -- withLogging :: (MonadIO m) => FastLoggerSettings -> (m a -> IO a) -> LoggingT m a -> m a

    makeLogger backend msg = do
      let settings = flbSettings backend
      let fltr = lsFilter settings
      let format = lsFormat settings
      let logger = flbTimedLogger backend
      when (checkLogLevel fltr msg) $ do
        logger $ formatLogMessage format msg

-- | Settings for syslog backend. This mostly reflects syslog API.
data SyslogSettings = SyslogSettings {
    ssFilter :: LogFilter         -- ^ Log messages filter
  , ssFormat :: F.Format         -- ^ Log message format. Usually you do not want to put time here,
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
-- @[{level}] {source}: {message}@
defaultSyslogFormat :: F.Format
defaultSyslogFormat = "[{level}] {source}: {message}"

data SyslogBackend = SyslogBackend {
    sbSettings :: SyslogSettings,
    sbIdent :: CString,
    sbTimeCache :: IO FormattedTime
  }

newSyslogBackend :: SyslogSettings -> IO SyslogBackend
newSyslogBackend settings = do
  ident <- newCString (ssIdent settings)
  tcache <- newTimeCache simpleTimeFormat'
  Syslog.openlog ident (ssOptions settings) (ssFacility settings)
  return $ SyslogBackend settings ident tcache

cleanupSyslogBackend :: SyslogBackend -> IO ()
cleanupSyslogBackend backend = do
    free $ sbIdent backend
    Syslog.closelog

instance IsLogBackend SyslogBackend where
    makeLogger backend msg = do
        let settings = sbSettings backend
        let fltr = ssFilter settings
            format = ssFormat settings
            facility = ssFacility settings
            tcache = sbTimeCache backend
        when (checkLogLevel fltr msg) $ do
          time <- tcache
          let str = formatLogMessage format msg time
          BSU.unsafeUseAsCStringLen (fromLogStr str) $
              Syslog.syslog (Just facility) (levelToPriority $ lmLevel msg)

      where
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

-- | Logging backend which writes all messages to the @Chan@
data ChanLoggerSettings = ChanLoggerSettings {
       clFilter :: LogFilter      -- ^ Log messages filter
     , clChan :: Chan LogMessage  -- ^ @Chan@ where write messages to
     }

instance IsLogBackend ChanLoggerSettings where
  makeLogger settings msg = do
    let fltr = clFilter settings
    when (checkLogLevel fltr msg) $ do
      liftIO $ writeChan (clChan settings) msg

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

