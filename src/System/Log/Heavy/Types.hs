{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, MultiParamTypeClasses, UndecidableInstances, AllowAmbiguousTypes, ScopedTypeVariables #-}

-- | This module contains generic types definition, along with some utilities.
module System.Log.Heavy.Types
  (
    LogSource, LogMessage (..), LogFilter,
    IsLogBackend (..), LogBackend (..), Logger,
    HasLogBackend (..), 
    -- LoggingT (LoggingT), runLoggingT,
    defaultLogFilter,
    splitString, splitDots,
    logMessage
  ) where

import Control.Monad.Reader
import Control.Monad.Logger (MonadLogger (..), LogLevel (..))
import Control.Monad.Base
import Control.Monad.Trans.Control
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
  makeLogger :: Logger b
  -- | Run LoggingT within some kind of IO monad
--   withLoggingB :: (MonadIO m)
--                => b             -- ^ Backend settings
--                -> (m a -> IO a) -- ^ Runner that allows to run this @m@ within @IO@
--                -> LoggingT m a  -- ^ Actions within @LoggingT@ monad
--                -> m a

class (Monad m, IsLogBackend b) => HasLogBackend b m where
  getLogBackend :: m b

-- class (HasLogBackend b m) => HasLogger b m where
--   getLogger :: m (Logger b)
--   getLogger = do
--     backend <- getLogBackend
--     return $ makeLogger

-- | A container for arbitrary logging backend.
-- You usually will use this similar to:
--
-- @
--  getLoggingSettings :: String -> LogBackend
--  getLoggingSettings "syslog" = LogBackend defaultsyslogsettings
-- @
data LogBackend = forall b. IsLogBackend b => LogBackend b

-- | Logging monad transformer.
-- newtype LoggingT m a = LoggingT {
--     runLoggingT_ :: ReaderT Logger m a
--   }
--   deriving (Functor, Applicative, Monad, MonadReader Logger, MonadTrans)
-- 
-- deriving instance MonadIO m => MonadIO (LoggingT m)
-- 
-- instance (Monad m, IsLogBackend backend) => HasLogBackend backend m where
--   getLogger = ask
-- 
-- instance MonadIO m => MonadBase IO (LoggingT m) where
--   liftBase = liftIO
-- 
-- instance MonadTransControl LoggingT where
--     type StT LoggingT a = StT (ReaderT Logger) a
--     liftWith = defaultLiftWith LoggingT runLoggingT_
--     restoreT = defaultRestoreT LoggingT
-- 
-- instance (MonadBaseControl IO m, MonadIO m) => MonadBaseControl IO (LoggingT m) where
--     type StM (LoggingT m) a = ComposeSt LoggingT m a
--     liftBaseWith     = defaultLiftBaseWith
--     restoreM         = defaultRestoreM
-- 
-- -- | Run logging monad
-- runLoggingT :: LoggingT m a -> Logger -> m a
-- runLoggingT actions logger = runReaderT (runLoggingT_ actions) logger

-- | Logging function
type Logger backend = backend -> LogMessage -> IO ()

textFromLogStr :: ToLogStr str => str -> TL.Text
textFromLogStr str = TL.fromStrict $ TE.decodeUtf8 $ fromLogStr $ toLogStr str

instance (Monad m, MonadIO m, HasLogBackend b m) => MonadLogger m where
  monadLoggerLog loc src level msg = do
      backend <- getLogBackend :: m b
      liftIO $ makeLogger backend $ LogMessage level src' loc (textFromLogStr msg) ()
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
logMessage :: forall b m. (HasLogBackend b m, MonadIO m) => LogMessage -> m ()
logMessage msg = do
  backend <- getLogBackend :: m b
  liftIO $ makeLogger backend msg

