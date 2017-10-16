{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, MultiParamTypeClasses, UndecidableInstances, FlexibleContexts #-}

-- | This is the main module of @heavy-logger@ package. You usually need to import only this module.
-- All generally required modules are re-exported.
--
-- For simple usage cases, you may also want to import System.Log.Heavy.Shortcuts module.
--
-- Example of usage is:
--
-- @
--  import System.Log.Heavy
--  import Data.Text.Format.Heavy
--  ...
--
--  withLogging backend id $ do liftIO $ putStr "Your name? "
--      liftIO $ hFlush stdout
--      name <- liftIO $ getLine
--      logMessage $ infoMessage "name was {}" (Single name)
--      liftIO $ putStrLn $ "Hello, " ++ name
-- @
--
-- See also @Test.hs@.
--
module System.Log.Heavy
  (
    -- * Reexports
    module System.Log.Heavy.Types,
    module System.Log.Heavy.Backends,
    withLogging
  ) where

import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Logger (LogLevel (..))
import Control.Exception.Lifted (bracket)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Format.Heavy as F

import System.Log.Heavy.Types
import System.Log.Heavy.Backends

withLogging :: (MonadBaseControl IO m, HasLogBackend m b, MonadIO m)
            => LoggingSettings
            -> m a
            -> m a
withLogging (LoggingSettings settings) actions = do
  bracket (liftIO $ initLogBackend settings)
          (liftIO . cleanupLogBackend)
          (const actions)

