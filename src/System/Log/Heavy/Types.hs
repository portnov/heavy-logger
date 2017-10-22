{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, MultiParamTypeClasses, UndecidableInstances, AllowAmbiguousTypes, ScopedTypeVariables, FunctionalDependencies, FlexibleContexts, ConstraintKinds #-}

-- | This module contains generic types definition, along with some utilities.
module System.Log.Heavy.Types
  (
    -- * Data types
    LogSource, Level (..), LogMessage (..), LogFilter, LogContextFrame (..), LogContext,
    IsLogBackend (..), LogBackendSettings (..), LoggingSettings (..),
    AnyLogBackend (..), LogContextFilter (..),
    Logger,SpecializedLogger, 
    HasLogBackend (..), HasLogContext (..), HasLogging,
    HasLogger (..),
    LoggingT (LoggingT), LoggingTState (..),
    -- * Standard severity levels
    debug_level, info_level, warn_level, error_level, fatal_level,
    -- * Conversion functions
    levelToLogLevel, levelToLogLevel,
    -- * Main functions
    logMessage',
    runLoggingT,
    applyBackend,
    defaultLogFilter,
    withLogVariable,
    -- * Utility functions
    splitString, splitDots,
  ) where

import Control.Monad.Reader
import Control.Monad.Logger (MonadLogger (..), LogLevel (..))
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Exception.Lifted (bracket)
import Data.String
import Language.Haskell.TH
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Format.Heavy as F
import System.Log.FastLogger
import qualified System.Posix.Syslog as Syslog

-- | Logging message severity level data type
data Level = Level {
    levelName :: T.Text            -- ^ Level name, like @"debug"@ or @"info"@
  , levelInt :: Int                -- ^ Integer level identifier. Comparation is based on these identifiers.
  , levelToPriority :: Syslog.Priority -- ^ Syslog equivalent of this level.
  } deriving (Eq)

instance Show Level where
  show l = T.unpack (levelName l)

instance Ord Level where
  compare l1 l2 = compare (levelInt l1) (levelInt l2)

-- | TRACE level is supposed to be used for development-stage debugging.
trace_level :: Level
trace_level = Level "TRACE" 600 Syslog.Debug

-- | DEBUG level is supposed to be used for debug logging that can be
-- enabled on production.
debug_level :: Level
debug_level = Level "DEBUG" 500 Syslog.Debug

-- | INFO level: some event occured.
info_level :: Level
info_level = Level "INFO" 400 Syslog.Info

-- | WARN level: something went wrong, but for now it will not affect
-- system's stability.
warn_level :: Level
warn_level = Level "WARN" 300 Syslog.Warning

-- | ERROR level: something went wrong.
error_level :: Level
error_level = Level "ERROR" 200 Syslog.Error

-- | FATAL level: something went terribly wrong, application is to be stopped.
fatal_level :: Level
fatal_level = Level "FATAL" 100 Syslog.Emergency

-- | Conversion function
levelToLogLevel :: Level -> LogLevel
levelToLogLevel l =
  case levelName l of
    "DEBUG" -> LevelDebug
    "INFO"  -> LevelInfo
    "WARN"  -> LevelWarn
    "ERROR" -> LevelError
    name -> LevelOther name

-- | Convertion function. Note that @LevelOther@ is
-- translated to integer level 210 and Syslog's Alert priority,
-- since in @monad-logger@ semantics any LevelOther is more severe
-- than LevelError.
logLevelToLevel :: LogLevel -> Level
logLevelToLevel LevelDebug = debug_level
logLevelToLevel LevelInfo  = info_level
logLevelToLevel LevelWarn  = warn_level
logLevelToLevel LevelError = error_level
logLevelToLevel (LevelOther name) = Level name 210 Syslog.Alert

-- | Log message source. This is usually a list of program module names,
-- for example @[\"System\", \"Log\", \"Heavy\", \"Types\"]@.
type LogSource = [String]

-- | Log message structure
data LogMessage = forall vars. F.VarContainer vars => LogMessage {
    lmLevel :: Level    -- ^ Log message level
  , lmSource :: LogSource  -- ^ Log message source (module)
  , lmLocation :: Loc      -- ^ Log message source (exact location). You usually
                           --   will want to use TH quotes to fill this.
  , lmFormatString :: TL.Text -- ^ Log message string format (in @Data.Text.Format.Heavy@ syntax)
  , lmFormatVars :: vars   -- ^ Log message substitution variables. Use @()@ if you do not have variables.
  , lmContext :: LogContext -- ^ Logging context. Authomatically filled by @logMessage@.
  }

-- | Log messages filter by source and level.
--
-- Semantics under this is that @(source, severity)@ pair allows to write
-- messages from @source@ of @severity@ (and all more important messages) to log.
type LogFilter = [(LogSource, Level)]

-- | Default log messages filter. This says pass all messages
-- of level Info or higher.
defaultLogFilter :: LogFilter
defaultLogFilter = [([], info_level)]

-- | One frame in logging context stack.
data LogContextFrame = LogContextFrame {
      lcfVariables :: [(TL.Text, F.Variable)] -- ^ Context variables
    , lcfFilter :: LogContextFilter           -- ^ Context events filter
  }
  deriving (Show)

-- | Events filter for specific logging context.
data LogContextFilter =
    NoChange          -- ^ Do not affect messages filtering
  | Include LogFilter -- ^ Allow messages by specific filter
  | Exclude LogFilter -- ^ Disallow messages by specific filter
  deriving (Eq, Show)

-- | Logging context stack
type LogContext = [LogContextFrame]

-- | Logging backend class.
class IsLogBackend b where
  -- | Logging backend settings data type
  data LogBackendSettings b

  -- | Create logger from backend
  makeLogger :: Logger b

  -- | Initialize logging backend from settings
  initLogBackend :: LogBackendSettings b -> IO b

  -- | Cleanup logging backend (release resources and so on)
  cleanupLogBackend :: b -> IO ()

  -- | Bracket function
  withLoggingB :: (MonadBaseControl IO m, MonadIO m)
            => LogBackendSettings b
            -> (b -> m a)
            -> m a
  withLoggingB settings actions = do
    bracket (liftIO $ initLogBackend settings)
            (liftIO . cleanupLogBackend)
            (actions)

-- | Container data type for representing arbitrary logging backend.
data AnyLogBackend = forall b. IsLogBackend b => AnyLogBackend b

-- | Constraint for monads in which it is possible to obtain logging backend.
class IsLogBackend b => HasLogBackend b m where
  getLogBackend :: m b

-- | A container for arbitrary logging backend.
-- You usually will use this similar to:
--
-- @
--  getLoggingSettings :: String -> LoggingSettings
--  getLoggingSettings "syslog" = LoggingSettings defaultsyslogsettings
-- @
data LoggingSettings = forall b. IsLogBackend b => LoggingSettings (LogBackendSettings b)

-- | State of @LoggingT@ monad
data LoggingTState = LoggingTState {
    ltsLogger :: SpecializedLogger
  , ltsContext :: LogContext
  }

-- | Logging monad transformer.
-- This is just a default implementation of @HasLogging@ interface.
-- Applications are free to use this or another implementation.
newtype LoggingT m a = LoggingT {
    runLoggingT_ :: ReaderT LoggingTState m a
  }
  deriving (Functor, Applicative, Monad, MonadReader LoggingTState, MonadTrans)

deriving instance MonadIO m => MonadIO (LoggingT m)

-- instance (Monad m, IsLogBackend backend) => HasLogBackend backend m where
--   getLogBackend = ask

instance MonadIO m => MonadBase IO (LoggingT m) where
  liftBase = liftIO

instance MonadTransControl LoggingT where
    type StT LoggingT a = StT (ReaderT LoggingTState) a
    liftWith = defaultLiftWith LoggingT runLoggingT_
    restoreT = defaultRestoreT LoggingT

instance (MonadBaseControl IO m, MonadIO m) => MonadBaseControl IO (LoggingT m) where
    type StM (LoggingT m) a = ComposeSt LoggingT m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM

-- | Run logging monad
runLoggingT :: LoggingT m a -> LoggingTState -> m a
runLoggingT actions context = runReaderT (runLoggingT_ actions) context

-- | Logging function
type Logger backend = backend -> LogMessage -> IO ()

-- | Logging function applied to concrete backend
type SpecializedLogger = LogMessage -> IO ()

-- | Type class for monads that can write logs
class Monad m => HasLogger m where
  getLogger :: m SpecializedLogger
  
  -- | Change logger to specified one locally
  localLogger :: SpecializedLogger -> m a -> m a

-- instance (Monad m, MonadReader SpecializedLogger m) => HasLogger m where
--   getLogger = ask
--   localLogger l = local (const l)

instance Monad m => HasLogger (LoggingT m) where
  getLogger = asks ltsLogger
  localLogger l actions = LoggingT $ ReaderT $ \lts -> runReaderT (runLoggingT_ actions) $ lts {ltsLogger = l}

-- | Apply logging backend locally.
applyBackend :: (IsLogBackend b, HasLogger m) => b -> m a -> m a
applyBackend b actions = do
  let logger = makeLogger b
  localLogger logger actions

-- | Type class for monads that store logging context
class Monad m => HasLogContext m where
  -- | Execute actions within logging context frame
  withLogContext :: LogContextFrame -> m a -> m a

  -- | Obtain currently active logging context stack
  getLogContext :: m LogContext

instance (Monad m) => HasLogContext (LoggingT m) where
  getLogContext = asks ltsContext

  withLogContext frame actions =
    LoggingT $ ReaderT $ \lts -> runReaderT (runLoggingT_ actions) $ lts {ltsContext = frame: ltsContext lts}

-- | Convinience constraint synonym.
type HasLogging m = (HasLogger m, HasLogContext m)

-- | Shortcut function to execute actions within logging context frame,
-- which contains only one variable
withLogVariable :: (HasLogContext m, F.Formatable v)
                => TL.Text -- ^ Variable name
                -> v       -- ^ Variable value
                -> m a     -- ^ Actions to execute within context frame
                -> m a
withLogVariable name value =
  withLogContext (LogContextFrame [(name, F.Variable value)] NoChange)

-- | Compatibility instance.
instance (Monad m, MonadIO m, HasLogging m) => MonadLogger m where
  monadLoggerLog loc src level msg = do
      logger <- getLogger
      context <- getLogContext
      liftIO $ logger $ LogMessage {
                          lmLevel = logLevelToLevel level,
                          lmSource = src',
                          lmLocation = loc,
                          lmFormatString = textFromLogStr msg,
                          lmFormatVars = (),
                          lmContext = context
                        }
    where
      src' = splitDots $ T.unpack src

      textFromLogStr :: ToLogStr str => str -> TL.Text
      textFromLogStr str = TL.fromStrict $ TE.decodeUtf8 $ fromLogStr $ toLogStr str

instance F.Formatable LogStr where
  formatVar fmt str = F.formatVar fmt $ fromLogStr str

-- | Simple implementation of splitting string by character.
splitString       :: Char -> String -> [String]
splitString _ ""  =  []
splitString c s   =  let (l, s') = break (== c) s
                 in  l : case s' of
                           []      -> []
                           (_:s'') -> splitString c s''

-- | Split string by dots
splitDots :: String -> [String]
splitDots = splitString '.'

-- | Log a message. This version is for monads that do not know about logging contexts.
logMessage' :: forall m. (HasLogger m, MonadIO m) => LogMessage -> m ()
logMessage' msg = do
  logger <- getLogger
  liftIO $ logger msg

