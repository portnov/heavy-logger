{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, MultiParamTypeClasses, UndecidableInstances #-}

module System.Log.Heavy.Backends
  (
  -- $description
  -- * Backends
  FastLoggerBackend,
  SyslogBackend,
  ChanLoggerBackend,
  ParallelBackend,
  Filtering, filtering,
  LogBackendSettings (..),
  -- * Default settings
  defStdoutSettings,
  defStderrSettings,
  defFileSettings,
  defaultSyslogSettings,
  defaultSyslogFormat,
  -- * Utilities for other backends implementation
  checkLogLevel, checkContextFilter, checkContextFilterM, logMessage
  ) where

import Control.Monad
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader
import Control.Monad.Logger (MonadLogger (..), LogLevel (..))
import Control.Concurrent
import Data.List (isPrefixOf)
import Data.Maybe
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

-- | Default settings for fast-logger stdout output
defStdoutSettings :: LogBackendSettings FastLoggerBackend
defStdoutSettings = FastLoggerSettings defaultLogFormat (FL.LogStdout FL.defaultBufSize)

-- | Default settings for fast-logger stderr output
defStderrSettings :: LogBackendSettings FastLoggerBackend
defStderrSettings = FastLoggerSettings defaultLogFormat (FL.LogStderr FL.defaultBufSize)

-- | Default settings for fast-logger file output.
-- This implies log rotation when log file size reaches 10Mb.
defFileSettings :: FilePath -> LogBackendSettings FastLoggerBackend
defFileSettings path = FastLoggerSettings defaultLogFormat (FL.LogFile spec FL.defaultBufSize)
  where spec = FL.FileLogSpec path (10*1024*1024) 3

-- | Fast-logger logging backend.
data FastLoggerBackend = FastLoggerBackend {
    flbSettings :: LogBackendSettings FastLoggerBackend,
    flbTimedLogger :: TimedFastLogger,
    flbCleanup :: IO ()
  }

instance IsLogBackend FastLoggerBackend where
    -- | Settings of fast-logger backend. This mostly reflects settings of fast-logger itself.
    data LogBackendSettings FastLoggerBackend = FastLoggerSettings {
        lsFormat :: F.Format -- ^ Log message format
      , lsType :: FL.LogType   -- ^ Fast-logger target settings
      }

    initLogBackend settings = do
        tcache <- newTimeCache simpleTimeFormat'
        (logger, cleanup) <- newTimedFastLogger tcache (lsType settings)
        return $ FastLoggerBackend settings logger cleanup

    cleanupLogBackend b = do
        flbCleanup b

    makeLogger backend msg = do
      let settings = flbSettings backend
      let format = lsFormat settings
      let logger = flbTimedLogger backend
      logger $ formatLogMessage format msg

-- | Default settings for syslog backend
defaultSyslogSettings :: LogBackendSettings SyslogBackend
defaultSyslogSettings = SyslogSettings defaultSyslogFormat "application" [] Syslog.User

-- | Default log message format fof syslog backend:
-- @[{level}] {source}: {message}@
defaultSyslogFormat :: F.Format
defaultSyslogFormat = "[{level}] {source}: {message}"

-- | Syslog logging backend.
data SyslogBackend = SyslogBackend {
    sbSettings :: LogBackendSettings SyslogBackend,
    sbIdent :: CString,
    sbTimeCache :: IO FormattedTime
  }

instance IsLogBackend SyslogBackend where
    -- | Settings for syslog backend. This mostly reflects syslog API.
    data LogBackendSettings SyslogBackend = SyslogSettings {
        ssFormat :: F.Format         -- ^ Log message format. Usually you do not want to put time here,
                                      --   because syslog writes time to log by itself by default.
      , ssIdent :: String             -- ^ Syslog source identifier. Usually the name of your program.
      , ssOptions :: [Syslog.Option]  -- ^ Syslog options
      , ssFacility :: Syslog.Facility -- ^ Syslog facility. It is usally User, if you are writing user-space
                                      --   program.
      }

    initLogBackend settings = do
        ident <- newCString (ssIdent settings)
        tcache <- newTimeCache simpleTimeFormat'
        Syslog.openlog ident (ssOptions settings) (ssFacility settings)
        return $ SyslogBackend settings ident tcache

    cleanupLogBackend backend = do
        free $ sbIdent backend
        Syslog.closelog

    makeLogger backend msg = do
        let settings = sbSettings backend
        let format = ssFormat settings
            facility = ssFacility settings
            tcache = sbTimeCache backend
        time <- tcache
        let str = formatLogMessage format msg time
        BSU.unsafeUseAsCStringLen (fromLogStr str) $
            Syslog.syslog (Just facility) (levelToPriority $ lmLevel msg)

-- | Logging backend which writes all messages to the @Chan@
data ChanLoggerBackend = ChanLoggerBackend {
       clChan :: Chan LogMessage  -- ^ @Chan@ where write messages to
     }

instance IsLogBackend ChanLoggerBackend where
  data LogBackendSettings ChanLoggerBackend = ChanLoggerSettings ChanLoggerBackend

  initLogBackend (ChanLoggerSettings backend) = return backend 

  cleanupLogBackend _ = return ()

  makeLogger settings msg = do
    liftIO $ writeChan (clChan settings) msg

-- | Logging backend that writes log messages to several other backends in parallel.
data ParallelBackend = ParallelBackend ![AnyLogBackend]

instance IsLogBackend ParallelBackend where
  data LogBackendSettings ParallelBackend = ParallelLogSettings [LoggingSettings]

  makeLogger (ParallelBackend list) msg =
    forM_ list $ \(AnyLogBackend backend) -> makeLogger backend msg

  initLogBackend (ParallelLogSettings list) = do
    backends <- do 
                forM list $ \(LoggingSettings settings) -> do
                  backend <- initLogBackend settings
                  return $ AnyLogBackend backend
    return $ ParallelBackend backends
      
  cleanupLogBackend (ParallelBackend list) =
    forM_ (reverse list) $ \(AnyLogBackend backend) -> cleanupLogBackend backend

-- | Messages filtering backend. This backend passes a message to underlying backend,
-- if this message conforms to specified filter.
data Filtering b = FilteringBackend (LogMessage -> Bool) b

-- | Specify filter as @LogFilter@.
filtering :: IsLogBackend b => LogFilter -> LogBackendSettings b -> LogBackendSettings (Filtering b)
filtering fltr b = Filtering (checkLogLevel fltr) b

instance IsLogBackend b => IsLogBackend (Filtering b) where
  data LogBackendSettings (Filtering b) = Filtering (LogMessage -> Bool) (LogBackendSettings b)

  makeLogger (FilteringBackend fltr backend) msg = do
    when (fltr msg) $ do
      makeLogger backend msg

  initLogBackend (Filtering fltr settings) = do
    backend <- initLogBackend settings
    return $ FilteringBackend fltr backend

  cleanupLogBackend (FilteringBackend _ b) = cleanupLogBackend b

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

-- | Check if message filter matches filters from logging context
checkContextFilter :: LogContext -> LogMessage -> Bool
checkContextFilter context msg =
  let includeFilters = [fltr | Include fltr <- map lcfFilter context]
      excludeFilters = [fltr | Exclude fltr <- map lcfFilter context]
      includeOk = null includeFilters || or [checkLogLevel fltr msg | fltr <- includeFilters]
      excludeOk = or [checkLogLevel fltr msg | fltr <- excludeFilters]
  in  includeOk && not excludeOk

-- | Check if message filter matches filters from logging context
checkContextFilterM :: HasLogContext m => LogMessage -> m Bool
checkContextFilterM msg = do
  context <- getLogContext
  return $ checkContextFilter context msg

-- | Log a message
logMessage :: forall m. (HasLogging m, MonadIO m) => LogMessage -> m ()
logMessage msg = do
  ok <- checkContextFilterM msg
  when ok $ do
    context <- getLogContext
    logger <- getLogger
    liftIO $ logger $ msg {lmContext = context ++ lmContext msg}

