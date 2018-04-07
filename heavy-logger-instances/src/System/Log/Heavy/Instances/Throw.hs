
-- | This module contains instances of MonadThrow, MonadCatch, MonadMsk type classes for
-- LoggingT monad transformer defined in the heavy-logger package.
--
module System.Log.Heavy.Instances.Throw where

import Control.Monad.Reader
import Control.Monad.Catch 
import System.Log.Heavy

instance MonadThrow m => MonadThrow (LoggingT m) where
  throwM e = LoggingT $ lift $ throwM e

instance MonadCatch m => MonadCatch (LoggingT m) where
  catch (LoggingT (ReaderT m)) c = LoggingT $ ReaderT $ \r -> m r `catch` \e -> runLoggingT (c e) r

instance MonadMask m => MonadMask (LoggingT m) where
  mask a = LoggingT $ ReaderT $ \lts -> mask $ \u -> runLoggingT (a $ q u) lts
    where q :: (m a -> m a) -> LoggingT m a -> LoggingT m a
          q u (LoggingT (ReaderT b)) = LoggingT (ReaderT (u . b))

  uninterruptibleMask a = LoggingT $ ReaderT $ \lts -> uninterruptibleMask $ \u -> runLoggingT (a $ q u) lts
    where q :: (m a -> m a) -> LoggingT m a -> LoggingT m a
          q u (LoggingT (ReaderT b)) = LoggingT (ReaderT (u . b))

