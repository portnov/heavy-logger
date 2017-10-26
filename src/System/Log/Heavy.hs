{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, MultiParamTypeClasses, UndecidableInstances, FlexibleContexts, Rank2Types, ScopedTypeVariables #-}
-- | This is the main module of @heavy-logger@ package. In most cases, you need
-- to import this module. You will also need other modules in specific cases.
-- All modules that are required always are re-exported by this module.
--
-- Example of usage is:
--
-- @
--  import System.Log.Heavy
--  import System.Log.Heavy.Shortcuts
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
-- Please refer to @examples/@ directory for compiling examples.
-- 
-- There are, in general, following ways to use this package:
-- 
-- * Use @LoggingT@ monad transformer. It can be the simplest, if you already have
--   monadic transformers stack of 1-2 transformers and you do not mind to add yet
--   another. With @LoggingT@, you do not need to write any adapter instances, since
--   @LoggingT@ is already an instance of all required classes. This implementation
--   automatically solves all threading-related problems, since in fact it does not
--   have any shared state.
--
-- * Use @System.Log.Heavy.IO@ module. If you do not have monadic transformers at all,
--   and your application works in pure IO, this may be the simplest way. However,
--   this is a bit fragile, because you have to be sure that you always call logging
--   functions only when logging state is initialized, i.e. within @withLoggingIO@
--   call. This implementation stores required state in thread-local storage.
--
-- * Implement required class instances for monadic stack that you already use in
--   your application. For example, if you already have something like
--   @ReaderT StateT ExceptT IO@, it will be probably better to add a couple of 
--   fields to StateT's state to track logging state, than change your stack to
--   @ReaderT StateT LoggingT ExceptT IO@. If you wish to store logging state in some
--   kind of shared storage (global IORef or whatever), then you should think about
--   thread-safety by yourself.
-- 
-- When you decided which monadic context you will use, you will call one of
-- @withLogging*@ functions to run the entire thing, and inside that you will construct
-- instances of @LogMessage@ type and call @logMessage@ or @logMessage'@ function on them
-- to actually log a message. You probably will want to use some shortcut functions to
-- construct @LogMessage@ instances and log them.  There are some provided
-- by this package:
--
-- * @System.Log.Heavy.Shortcuts@ module exports simple functions, that can be used
--   in simple cases, when you do not want to write or check message source.
--
-- * @System.Log.Heavy.TH@ module exports TH macros, which correctly fill message
--   source and location.
--
module System.Log.Heavy
  (
    -- * Reexports
    module System.Log.Heavy.Types,
    module System.Log.Heavy.Level,
    module System.Log.Heavy.LoggingT,
    module System.Log.Heavy.Backends,
    module System.Log.Heavy.Backends.Dynamic,
    withLogging, withLoggingF, withLoggingT,
    isLevelEnabledByBackend, isLevelEnabled,
  ) where

import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Exception.Lifted (bracket)
import qualified Data.Text.Lazy as TL

import System.Log.Heavy.Types
import System.Log.Heavy.Level
import System.Log.Heavy.LoggingT
import System.Log.Heavy.Backends
import System.Log.Heavy.Backends.Dynamic

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
      in  runLoggingT actions $ LoggingTState logger (AnyLogBackend backend) []

-- | Check if logging of events of specified level from specified source
-- is enabled by backend.
--
-- This function assumes that if some events filtering is enabled by the
-- backend, it does not depend on message text, only on source and 
-- severity level.
isLevelEnabledByBackend :: forall m. (Monad m, MonadIO m, HasLogBackend AnyLogBackend m) => LogSource -> Level -> m Bool
isLevelEnabledByBackend src level = do
  backend <- getLogBackend :: m AnyLogBackend
  let msg = LogMessage level src undefined TL.empty () []
  liftIO $ wouldWriteMessage backend msg

-- | Check if logging of events of specified level from specified source
-- is enabled by both context and backend filter.
--
-- This function assumes that if some events filtering is enabled by the
-- backend, it does not depend on message text, only on source and 
-- severity level.
isLevelEnabled :: forall m. (Monad m, MonadIO m, HasLogBackend AnyLogBackend m, HasLogContext m) => LogSource -> Level -> m Bool
isLevelEnabled src level = do
  let msg = LogMessage level src undefined TL.empty () []
  backend <- getLogBackend :: m AnyLogBackend
  isEnabledByBackend <- liftIO $ wouldWriteMessage backend msg
  isEnabledByContext <- checkContextFilterM msg
  return $ isEnabledByContext && isEnabledByBackend

