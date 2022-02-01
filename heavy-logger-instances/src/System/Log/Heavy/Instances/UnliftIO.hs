-- | This module contains the instance of Control.Monad.IO.Unlift.MonadUnliftIO
-- for LoggingT monad transformer defined in the heavy-logger package.
--
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.Log.Heavy.Instances.UnliftIO where

import Control.Monad.IO.Unlift
import System.Log.Heavy

deriving instance MonadUnliftIO m => MonadUnliftIO (LoggingT m)

