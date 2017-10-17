{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, MultiParamTypeClasses, UndecidableInstances, AllowAmbiguousTypes, ScopedTypeVariables, FunctionalDependencies, FlexibleContexts, ConstraintKinds #-}

-- | This module contains generic types definition, along with some utilities.
module System.Log.Heavy.Types
  (
    LogSource, LogMessage (..), LogFilter,
    IsLogBackend (..), LoggingSettings (..), Logger,
    -- HasLogBackend (..),
    SpecializedLogger, HasLogger (..),
    applyBackend,
    LoggingT (LoggingT), runLoggingT,
    defaultLogFilter,
    splitString, splitDots,
    logMessage
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
import System.Log.FastLogger
import qualified Data.Text.Format.Heavy as F

-- | Log message source. This is usually a list of program module names,
-- for example @[\"System\", \"Log\", \"Heavy\", \"Types\"]@.
type LogSource = [String]

-- | Log message structure
data LogMessage = forall vars. F.VarContainer vars => LogMessage {
    lmLevel :: LogLevel    -- ^ Log message level
  , lmSource :: LogSource  -- ^ Log message source (module)
  , lmLocation :: Loc      -- ^ Log message source (exact location). You usually
                           --   will want to use TH quotes to fill this.
  , lmFormatString :: TL.Text -- ^ Log message string format (in @Data.Text.Format.Heavy@ syntax)
  , lmFormatVars :: vars   -- ^ Log message substitution variables. Use @()@ if you do not have variables.
  }

-- | Log messages filter by source and level.
--
-- Semantics under this is that @(source, severity)@ pair allows to write
-- messages from @source@ of @severity@ (and all more important messages) to log.
type LogFilter = [(LogSource, LogLevel)]

-- | Default log messages filter. This says pass all messages
-- of level Info or higher.
defaultLogFilter :: LogFilter
defaultLogFilter = [([], LevelInfo)]

-- | Logging backend class.
class IsLogBackend b where
  data LogBackendSettings b

  makeLogger :: Logger b

  initLogBackend :: LogBackendSettings b -> IO b
  -- getLoggerSettings :: b -> LogBackendSettings b
  cleanupLogBackend :: b -> IO ()

  withLoggingB :: (MonadBaseControl IO m, MonadIO m)
            => LogBackendSettings b
            -> (b -> m a)
            -> m a
  withLoggingB (settings) actions = do
    bracket (liftIO $ initLogBackend settings)
            (liftIO . cleanupLogBackend)
            (actions)

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

-- | Logging monad transformer.
newtype LoggingT m a = LoggingT {
    runLoggingT_ :: ReaderT SpecializedLogger m a
  }
  deriving (Functor, Applicative, Monad, MonadReader SpecializedLogger, MonadTrans)

deriving instance MonadIO m => MonadIO (LoggingT m)

-- instance (Monad m, IsLogBackend backend) => HasLogBackend backend m where
--   getLogBackend = ask

instance MonadIO m => MonadBase IO (LoggingT m) where
  liftBase = liftIO

instance MonadTransControl LoggingT where
    type StT LoggingT a = StT (ReaderT SpecializedLogger) a
    liftWith = defaultLiftWith LoggingT runLoggingT_
    restoreT = defaultRestoreT LoggingT

instance (MonadBaseControl IO m, MonadIO m) => MonadBaseControl IO (LoggingT m) where
    type StM (LoggingT m) a = ComposeSt LoggingT m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM

-- | Run logging monad
runLoggingT :: LoggingT m a -> SpecializedLogger -> m a
runLoggingT actions logger = runReaderT (runLoggingT_ actions) logger

-- | Logging function
type Logger backend = backend -> LogMessage -> IO ()

type SpecializedLogger = LogMessage -> IO ()

-- data AnyLogger = forall backend. IsLogBackend backend => AnyLogger (Logger backend)

-- class (Monad m, IsLogBackend backend) => HasLogger m where
--   getLogger :: m SpecializedLogger
-- 
--   applyBackend :: IsLogBackend backend => backend -> m a -> m a

class Monad m => HasLogger m where
  getLogger :: m SpecializedLogger
  localLogger :: SpecializedLogger -> m a -> m a

instance (Monad m, MonadReader SpecializedLogger m) => HasLogger m where
  getLogger = ask
  localLogger l = local (const l)

applyBackend :: (IsLogBackend b, HasLogger m) => b -> m a -> m a
applyBackend b actions = do
  let logger = makeLogger b
  localLogger logger actions

-- instance (Monad m, MonadIO m, HasLogBackend b m) => HasLogger b m where
--   getLogger = do
--     backend <- ask
--     return $ (makeLogger :: Logger b) backend
-- 
--   applyBackend b actions = local (const b) actions

textFromLogStr :: ToLogStr str => str -> TL.Text
textFromLogStr str = TL.fromStrict $ TE.decodeUtf8 $ fromLogStr $ toLogStr str

instance (Monad m, MonadIO m, HasLogger m) => MonadLogger m where
  monadLoggerLog loc src level msg = do
      logger <- getLogger
      liftIO $ logger $ LogMessage level src' loc (textFromLogStr msg) ()
    where
      src' = splitDots $ T.unpack src

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

-- | Log a message
logMessage :: forall m. (HasLogger m, MonadIO m) => LogMessage -> m ()
logMessage msg = do
  logger <- getLogger
  liftIO $ logger msg

