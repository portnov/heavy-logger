{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, MultiParamTypeClasses, UndecidableInstances, AllowAmbiguousTypes, ScopedTypeVariables, FunctionalDependencies, FlexibleContexts, ConstraintKinds #-}

-- | This module contains types and functions for log message severities.
module System.Log.Heavy.Level
  ( -- * Data types
   Level (..),
   -- * Utility functions
   levelToLogLevel, logLevelToLevel,
   parseLevel,
   -- * Standard severity levels
   debug_level, info_level, warn_level, error_level, fatal_level,
   disable_logging
  ) where

import Control.Monad.Logger (LogLevel (..))
import qualified Data.Text as T
import qualified System.Posix.Syslog as Syslog

-- | Logging message severity level data type
data Level = Level {
    levelName :: T.Text            -- ^ Level name, like @"debug"@ or @"info"@
  , levelInt :: Int                -- ^ Integer level identifier. Comparation is based on these identifiers.
  , levelToPriority :: Syslog.Priority -- ^ Syslog equivalent of this level.
  } deriving (Eq)

instance Show Level where
  show l = T.unpack (levelName l)

instance Ord Level where
  compare l1 l2 = compare (levelInt l1) (levelInt l2)

-- | TRACE level is supposed to be used for development-stage debugging.
trace_level :: Level
trace_level = Level "TRACE" 600 Syslog.Debug

-- | DEBUG level is supposed to be used for debug logging that can be
-- enabled on production.
debug_level :: Level
debug_level = Level "DEBUG" 500 Syslog.Debug

-- | INFO level: some event occured.
info_level :: Level
info_level = Level "INFO" 400 Syslog.Info

-- | WARN level: something went wrong, but for now it will not affect
-- system's stability.
warn_level :: Level
warn_level = Level "WARN" 300 Syslog.Warning

-- | ERROR level: something went wrong.
error_level :: Level
error_level = Level "ERROR" 200 Syslog.Error

-- | FATAL level: something went terribly wrong, application is to be stopped.
fatal_level :: Level
fatal_level = Level "FATAL" 100 Syslog.Emergency

-- | DISABLED level. This has integer identifier of 0, which is supposed to
-- be less than any other level. This value can be used to disable logging at
-- all.
disable_logging :: Level
disable_logging = Level "DISABLED" 0 Syslog.Emergency

-- | Conversion function
levelToLogLevel :: Level -> LogLevel
levelToLogLevel l =
  case levelName l of
    "DEBUG" -> LevelDebug
    "INFO"  -> LevelInfo
    "WARN"  -> LevelWarn
    "ERROR" -> LevelError
    name -> LevelOther name

-- | Convertion function. Note that @LevelOther@ is
-- translated to integer level 210 and Syslog's Alert priority,
-- since in @monad-logger@ semantics any LevelOther is more severe
-- than LevelError.
logLevelToLevel :: LogLevel -> Level
logLevelToLevel LevelDebug = debug_level
logLevelToLevel LevelInfo  = info_level
logLevelToLevel LevelWarn  = warn_level
logLevelToLevel LevelError = error_level
logLevelToLevel (LevelOther name) = Level name 210 Syslog.Alert

-- | Detect @Level@ from it's name. This function
-- is not case-sensitive.
parseLevel :: [Level] -- ^ List of recognized levels
           -> T.Text  -- ^ Level name to find
           -> Maybe Level -- ^ Nothing if no match found
parseLevel knownLevels str = go knownLevels
  where
    go [] = Nothing
    go (l:ls)
      | T.toCaseFold (levelName l) == needle = Just l
      | otherwise = go ls

    needle = T.toCaseFold str
    
