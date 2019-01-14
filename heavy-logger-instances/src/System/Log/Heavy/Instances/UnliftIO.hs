-- | This module contains the instance of Control.Monad.IO.Unlift.MonadUnliftIO
-- for LoggingT monad transformer defined in the heavy-logger package.
--
module System.Log.Heavy.Instances.UnliftIO where

import Control.Monad.IO.Unlift
import System.Log.Heavy

instance MonadUnliftIO m => MonadUnliftIO (LoggingT m) where
  askUnliftIO = do
    unliftIOReaderT <- LoggingT askUnliftIO -- UnliftIO (ReaderT LoggingTState m)
    -- unliftIO unliftIOReaderT :: forall a. ReaderT LoggingTState m a => IO a
    return $ UnliftIO $ \(LoggingT readerTa) -> unliftIO unliftIOReaderT readerTa

