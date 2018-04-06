{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, MultiParamTypeClasses, UndecidableInstances, AllowAmbiguousTypes, ScopedTypeVariables, FunctionalDependencies, FlexibleContexts, ConstraintKinds #-}

-- | This module contains generic types definition, along with some utilities.
module System.Log.Heavy.Types
  (
    -- * Data types
    LogSource, LogMessage (..), LogFilter, LogContextFrame (..), LogContext,
    IsLogBackend (..), LogBackendSettings (..), LoggingSettings (..),
    AnyLogBackend (..), LogContextFilter (..),
    include, exclude, noChange,
    Logger,SpecializedLogger, 
    HasLogBackend (..), HasLogContext (..), HasLogging,
    HasLogger (..),
    -- * Main functions
    logMessage',
    applyBackend,
    defaultLogFilter,
    withLogVariable,
    -- * Utility functions
    splitString, splitDots,
  ) where

import Control.Monad.Reader
import Control.Monad.Logger (MonadLogger (..))
import Control.Monad.Trans.Control
import Control.Exception.Lifted (bracket)
import Language.Haskell.TH
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Format.Heavy as F
import System.Log.FastLogger

import System.Log.Heavy.Level

-- | Log message source. This is usually a list of program module names,
-- for example @[\"System\", \"Log\", \"Heavy\", \"Types\"]@.
type LogSource = [String]

-- | Log message structure. You usually will want to use some sort
-- of shortcut function to create messages. There are some provided
-- by this package:
--
-- * @System.Log.Heavy.Shortcuts@ module exports simple functions, that can be used
--   in simple cases, when you do not want to write or check message source.
--
-- * @System.Log.Heavy.TH@ module exports TH macros, which correctly fill message
--   source and location.
--
data LogMessage = forall vars. F.ClosedVarContainer vars => LogMessage {
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
  LogContextFilter {
      setInclude :: Maybe LogFilter  -- ^ Positive filter (include specified messages)
    , setExclude :: Maybe LogFilter  -- ^ Negative filter (exclude specified messages)
  }
  deriving (Eq, Show)

-- | Do not affect context filter settings
noChange :: LogContextFilter
noChange = LogContextFilter Nothing Nothing

-- | Create filter which includes only specified messages
include :: LogFilter -> LogContextFilter
include f = LogContextFilter (Just f) Nothing

-- | Create filter which just excludes specified messages
exclude :: LogFilter -> LogContextFilter
exclude f = LogContextFilter Nothing (Just f)

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

  -- | Should return True if the specified message would be
  -- actually written to the log. Default implementation 
  -- always returns True.
  wouldWriteMessage :: b -> LogMessage -> IO Bool
  wouldWriteMessage _ _ = return True

  -- | Cleanup logging backend (release resources and so on)
  cleanupLogBackend :: b -> IO ()

  -- | Bracket function. Concrete implementations usually
  -- do not have to override default implementation.
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

instance IsLogBackend AnyLogBackend where
  data LogBackendSettings AnyLogBackend =
    AnyLogBackendSettings LoggingSettings

  makeLogger (AnyLogBackend backend) = makeLogger backend

  wouldWriteMessage (AnyLogBackend backend) msg =
    wouldWriteMessage backend msg

  initLogBackend (AnyLogBackendSettings (LoggingSettings settings)) =
    AnyLogBackend `fmap` initLogBackend settings

  cleanupLogBackend (AnyLogBackend backend) = cleanupLogBackend backend

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

-- GHC will not be able to select instance for LoggingT.
-- instance (Monad m, HasLogBackend b m) => HasLogBackend AnyLogBackend m where
--   getLogBackend = do
--     backend <- getLogBackend :: m b
--     return $ AnyLogBackend backend

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
  withLogContext (LogContextFrame [(name, F.Variable value)] noChange)

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

