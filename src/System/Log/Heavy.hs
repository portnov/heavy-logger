{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, MultiParamTypeClasses, UndecidableInstances #-}

-- | This is the main module of @heavy-logger@ package. You usually need to import only this module.
-- All generally required modules are re-exported.
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
    withLogging,
    -- * Some shortcuts
    errorMessage, infoMessage, debugMessage
  ) where

import Control.Monad.Trans
import Control.Monad.Logger (LogLevel (..))
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Format.Heavy as F

import System.Log.Heavy.Types
import System.Log.Heavy.Backends

-- | Run LoggingT monad within some kind of IO monad.
withLogging :: MonadIO m
            => LogBackend    -- ^ Logging backend settings
            -> (m a -> IO a) -- ^ Runner to run @m@ within @IO@. 
                             --   For example this may be @runReader@ or @evalState@.
                             --   Use @id@ for case when @m@ is @IO@.
            -> LoggingT m a  -- ^ Actions within @LoggingT@ monad.
            -> m a
withLogging (LogBackend b) = withLoggingB b

-- | Message stub with Info severity.
infoMessage :: F.VarContainer vars => TL.Text -> vars -> LogMessage
infoMessage fmt vars = LogMessage LevelInfo [] undefined fmt vars

-- | Message stub with Debug severity.
debugMessage :: F.VarContainer vars => TL.Text -> vars -> LogMessage
debugMessage fmt vars = LogMessage LevelDebug [] undefined fmt vars

-- | Message stub with Error severity.
errorMessage :: F.VarContainer vars => TL.Text -> vars -> LogMessage
errorMessage fmt vars = LogMessage LevelError [] undefined fmt vars

