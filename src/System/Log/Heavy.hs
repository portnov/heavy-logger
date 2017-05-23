{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving #-}

module System.Log.Heavy where

import Control.Applicative
import Control.Monad.Reader
import Control.Exception
import Data.String
import Data.Monoid
import Data.List
import Language.Haskell.TH
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BSU
import System.Log.FastLogger as F
import qualified System.Posix.Syslog as Syslog

data LogLevel =
    Emergency
  | Alert
  | Critical
  | Error
  | Warning
  | Notice
  | Info
  | Debug
  deriving (Eq, Show, Ord)

type LogSource = [String]

data LogMessage = LogMessage {
    lmLevel :: LogLevel,
    lmSource :: LogSource,
    lmLocation :: Loc,
    lmString :: LogStr
  }

data FormatItem =
    FLevel
  | FSource
  | FTime
  | FMessage
  | FString B.ByteString
  deriving (Eq, Show)

instance IsString FormatItem where
  fromString str = FString $ fromString str

type LogFormat = [FormatItem]

instance IsString LogFormat where
  fromString str = [fromString str]

defaultLogFormat :: LogFormat
defaultLogFormat = [FTime, FLevel, FSource, FMessage]

formatLogMessage :: LogFormat -> LogMessage -> FormattedTime -> LogStr
formatLogMessage format m ftime = mconcat $ intersperse (toLogStr (" " :: B.ByteString)) $ map go format
  where
    go :: FormatItem -> LogStr
    go FLevel = toLogStr $ show $ lmLevel m
    go FSource = toLogStr $ intercalate "." $ lmSource m
    go FTime = toLogStr ftime
    go FMessage = lmString m
    go (FString s) = toLogStr s

type LogFilter = [(LogSource, LogLevel)]

defaultLogFilter :: LogFilter
defaultLogFilter = [([], Info)]

checkLogLevel :: LogFilter -> LogMessage -> Bool
checkLogLevel fltr m = any fits fltr
  where
    fits (src, level) = src `isPrefixOf` lmSource m && lmLevel m <= level

class IsLogBackend b where
  withLoggingB :: (MonadIO m) => b -> (m a -> IO a) -> LoggingT m a -> m a

data LogBackend = forall b. IsLogBackend b => LogBackend b

withLogging :: MonadIO m => LogBackend -> (m a -> IO a) -> LoggingT m a -> m a
withLogging (LogBackend b) = withLoggingB b

data FastLoggerSettings = FastLoggerSettings {
    lsFilter :: LogFilter,
    lsFormat :: LogFormat,
    lsType :: F.LogType
  }

defStdoutSettings :: FastLoggerSettings
defStdoutSettings = FastLoggerSettings defaultLogFilter defaultLogFormat (F.LogStdout F.defaultBufSize)

defStderrSettings :: FastLoggerSettings
defStderrSettings = FastLoggerSettings defaultLogFilter defaultLogFormat (F.LogStderr F.defaultBufSize)

defFileSettings :: FilePath -> FastLoggerSettings
defFileSettings path = FastLoggerSettings defaultLogFilter defaultLogFormat (F.LogFile spec F.defaultBufSize)
  where spec = F.FileLogSpec path (10*1024*1024) 3

newtype LoggingT m a = LoggingT {
    runLoggingT :: ReaderT Logger m a
  }
  deriving (Functor, Applicative, Monad, MonadReader Logger)

deriving instance MonadIO m => MonadIO (LoggingT m)

type Logger = LogMessage -> IO ()

logMessage :: (MonadIO m) => LogMessage -> LoggingT m ()
logMessage m = do
  logger <- ask
  liftIO $ logger m

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

data SyslogSettings = SyslogSettings {
    ssFilter :: LogFilter,
    ssFormat :: LogFormat,
    ssIdent :: String,
    ssOptions :: [Syslog.Option],
    ssFacility :: Syslog.Facility
  }

defaultSyslogSettings :: SyslogSettings
defaultSyslogSettings = SyslogSettings defaultLogFilter defaultSyslogFormat "application" [] Syslog.User

defaultSyslogFormat :: LogFormat
defaultSyslogFormat = [FLevel, FSource, FMessage]

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
        levelToPriority Emergency = Syslog.Emergency
        levelToPriority Alert = Syslog.Alert
        levelToPriority Critical = Syslog.Critical
        levelToPriority Error = Syslog.Error
        levelToPriority Warning = Syslog.Warning
        levelToPriority Notice = Syslog.Notice
        levelToPriority Info = Syslog.Info
        levelToPriority Debug = Syslog.Debug

