{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, MultiParamTypeClasses, UndecidableInstances #-}
-- | This module contains implementation of most commonly used logging backends.
-- You can write your own backends, by implementing an instance of @IsLogBackend@
-- type class.
module System.Log.Heavy.Backends
  (
  -- $description
  -- * Backends
  FastLoggerBackend,
  SyslogBackend,
  ChanLoggerBackend,
  ParallelBackend,
  NullBackend,
  Filtering, filtering, excluding,
  LogBackendSettings (..),
  -- * Default settings
  defStdoutSettings,
  defStderrSettings,
  defFileSettings,
  defaultSyslogSettings,
  defaultSyslogFormat
  ) where

import Control.Monad
import Control.Monad.Trans (liftIO)
import Control.Concurrent
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Text.Format.Heavy as F
import qualified System.Posix.Syslog as Syslog
import System.Log.FastLogger as FL
import Foreign.C.String (CString, newCString)
import Foreign.Marshal.Alloc (free)

import System.Log.Heavy.Types
import System.Log.Heavy.Level
import System.Log.Heavy.Format
import System.Log.Heavy.Util

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
-- * Null backend. This discards all messages. Can be used to disable logging.
--
-- There are also some backend combinators:
--
-- * Filtering - passes messages, that match specified filter,
--   to underlying backend.
--
-- * FilteringM - similar to Filtering, but allows to change the filter in runtime.
--
-- * Parallel - writes messages to several backends in parallel.
--
-- * Dynamic - allows to change underlying backend or its settings in runtime.
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
  data LogBackendSettings ChanLoggerBackend =
    ChanLoggerSettings (Chan LogMessage)

  initLogBackend (ChanLoggerSettings chan) =
    return $ ChanLoggerBackend chan

  cleanupLogBackend _ = return ()

  makeLogger backend msg = do
    liftIO $ writeChan (clChan backend) msg

-- | Logging backend that writes log messages to several other backends in parallel.
data ParallelBackend = ParallelBackend ![AnyLogBackend]

instance IsLogBackend ParallelBackend where
  data LogBackendSettings ParallelBackend = ParallelLogSettings [LoggingSettings]

  wouldWriteMessage (ParallelBackend list) msg = do
    results <- sequence [wouldWriteMessage backend msg | backend <- list]
    return $ or results

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

-- | Exclude messages by filter.
excluding :: IsLogBackend b => LogFilter -> LogBackendSettings b -> LogBackendSettings (Filtering b)
excluding fltr b = Filtering ex b
  where
    ex msg = not $ checkContextFilter' [LogContextFilter Nothing (Just fltr)] (lmSource msg) (lmLevel msg)

instance IsLogBackend b => IsLogBackend (Filtering b) where
  data LogBackendSettings (Filtering b) = Filtering (LogMessage -> Bool) (LogBackendSettings b)

  wouldWriteMessage (FilteringBackend fltr _) msg = do
    return $ fltr msg

  makeLogger (FilteringBackend fltr backend) msg = do
    when (fltr msg) $ do
      makeLogger backend msg

  initLogBackend (Filtering fltr settings) = do
    backend <- initLogBackend settings
    return $ FilteringBackend fltr backend

  cleanupLogBackend (FilteringBackend _ b) = cleanupLogBackend b

-- | Null logging backend, which discards all messages
-- (passes them to @/dev/null@, if you wish).
-- This can be used to disable logging.
data NullBackend = NullBackend

instance IsLogBackend NullBackend where
  data LogBackendSettings NullBackend = NullLogSettings

  wouldWriteMessage _ _ = return False

  makeLogger _ _ = return ()

  initLogBackend _ = return NullBackend

  cleanupLogBackend _ = return ()

