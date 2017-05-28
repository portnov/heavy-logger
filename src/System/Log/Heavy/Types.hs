{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, MultiParamTypeClasses, UndecidableInstances #-}

module System.Log.Heavy.Types where

import Control.Monad.Reader
import Control.Monad.Logger (MonadLogger (..), LogLevel (..))
import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.String
import Language.Haskell.TH
import qualified Data.Text as T
import System.Log.FastLogger as F

type LogSource = [String]

data LogMessage = LogMessage {
    lmLevel :: LogLevel,
    lmSource :: LogSource,
    lmLocation :: Loc,
    lmString :: LogStr
  }

type LogFilter = [(LogSource, LogLevel)]

defaultLogFilter :: LogFilter
defaultLogFilter = [([], LevelInfo)]

class IsLogBackend b where
  withLoggingB :: (MonadIO m) => b -> (m a -> IO a) -> LoggingT m a -> m a

data LogBackend = forall b. IsLogBackend b => LogBackend b

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

logMessage :: (MonadIO m) => LogMessage -> LoggingT m ()
logMessage m = do
  logger <- ask
  liftIO $ logger m

