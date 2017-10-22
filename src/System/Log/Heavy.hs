{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, MultiParamTypeClasses, UndecidableInstances, FlexibleContexts, Rank2Types #-}

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
--  withLoggingT settings $ do
--      liftIO $ putStr "Your name? "
--      liftIO $ hFlush stdout
--      name <- liftIO $ getLine
--      info "name was {}" (Single name)
--      liftIO $ putStrLn $ "Hello, " ++ name
-- @
--
-- See also @Test.hs@.
--
module System.Log.Heavy
  (
    -- * Reexports
    module System.Log.Heavy.Types,
    module System.Log.Heavy.Level,
    module System.Log.Heavy.Backends,
    withLogging, withLoggingF, withLoggingT
  ) where

import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Logger (LogLevel (..))
import Control.Exception.Lifted (bracket)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Format.Heavy as F

import System.Log.Heavy.Types
import System.Log.Heavy.Level
import System.Log.Heavy.Backends

-- | Execute actions with logging backend.
-- This is mostly an utility function to be used to construct custom 
-- logging frameworks for custom monad transformer stacks.
withLoggingF :: (MonadBaseControl IO m, MonadIO m)
            => LoggingSettings                        -- ^ Settings of arbitrary logging backend.
            -> (forall b. IsLogBackend b => b -> m a) -- ^ Actions to execute with logging backend.
                                                      --   Note that this type declaration binds argument
                                                      --   to work with *any* implementation of backend.
            -> m a
withLoggingF (LoggingSettings settings) actions = withLoggingB settings actions

-- | Execute actions with logging.
-- This function can be useful for monad stacks that store logging backend
-- in State-like structure.
withLogging :: (MonadBaseControl IO m, MonadIO m, HasLogger m)
            => LoggingSettings -- ^ Settings of arbitrary logging backend
            -> m a             -- ^ Actions to be executed
            -> m a
withLogging (LoggingSettings settings) actions = 
    bracket (liftIO $ initLogBackend settings)
            (liftIO . cleanupLogBackend)
            (\b -> applyBackend b actions)

-- | Execute actions with logging.
-- This function is most convinient if you use @LoggingT@ as
-- @HasLogging@ implementation.
withLoggingT :: (MonadBaseControl IO m, MonadIO m)
                  => LoggingSettings   -- ^ Settings of arbitrary logging backend
                  -> LoggingT m a      -- ^ Actions to be executed
                  -> m a
withLoggingT (LoggingSettings settings) actions =
  withLoggingB settings $ \backend ->
      let logger = makeLogger backend
      in  runLoggingT actions $ LoggingTState logger []

