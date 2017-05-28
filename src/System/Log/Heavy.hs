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
defaultLogFilter = [([], LevelInfo)]

checkLogLevel :: LogFilter -> LogMessage -> Bool
checkLogLevel fltr m = any fits fltr
  where
    fits (src, level) = src `isPrefixOf` lmSource m && lmLevel m >= level

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
  deriving (Functor, Applicative, Monad, MonadReader Logger, MonadTrans)

deriving instance MonadIO m => MonadIO (LoggingT m)

instance MonadIO m => MonadBase IO (LoggingT m) where
  liftBase = liftIO

instance MonadTransControl LoggingT where
    type StT LoggingT a = StT (ReaderT Logger) a
    liftWith = defaultLiftWith LoggingT runLoggingT
    restoreT = defaultRestoreT LoggingT

instance (MonadBaseControl IO m, MonadIO m) => MonadBaseControl IO (LoggingT m) where
    type StM (LoggingT m) a = ComposeSt LoggingT m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM

runLoggingTReader :: LoggingT m a -> Logger -> m a
runLoggingTReader actions logger = runReaderT (runLoggingT actions) logger

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

instance MonadIO m => MonadLogger (LoggingT m) where
  monadLoggerLog loc src level msg =
      logMessage $ LogMessage level src' loc (toLogStr msg)
    where
      src' = splitDots $ T.unpack src

splitString       :: Char -> String -> [String]
splitString _ ""  =  []
splitString c s   =  let (l, s') = break (== c) s
                 in  l : case s' of
                           []      -> []
                           (_:s'') -> splitString c s''

splitDots :: String -> [String]
splitDots = splitString '.'

