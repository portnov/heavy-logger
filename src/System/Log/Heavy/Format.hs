{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, MultiParamTypeClasses, UndecidableInstances #-}

module System.Log.Heavy.Format where

import Control.Monad.Logger (MonadLogger (..), LogLevel (..))
import Data.List (intersperse, intercalate)
import Data.String
import qualified Data.Text as T
import qualified Data.ByteString as B
import System.Log.FastLogger as F

import System.Log.Heavy.Types

data FormatItem =
    FLevel
  | FSource
  | FLocation
  | FTime
  | FMessage
  | FString B.ByteString
  deriving (Eq, Show)

instance IsString FormatItem where
  fromString str = FString $ fromString str

type LogFormat = [FormatItem]

instance IsString LogFormat where
  fromString str = [fromString str]

defaultLogFormat :: LogFormat
defaultLogFormat = [FTime, FLevel, FSource, FMessage, FString "\n"]

formatLogMessage :: LogFormat -> LogMessage -> FormattedTime -> LogStr
formatLogMessage format m ftime = mconcat $ intersperse (toLogStr (" " :: B.ByteString)) $ map go format
  where
    go :: FormatItem -> LogStr
    go FLevel = toLogStr $ showLevel $ lmLevel m
    go FSource = toLogStr $ intercalate "." $ lmSource m
    go FLocation = toLogStr $ show $ lmLocation m
    go FTime = toLogStr ftime
    go FMessage = lmString m
    go (FString s) = toLogStr s

    showLevel LevelDebug = "debug"
    showLevel LevelInfo = "info"
    showLevel LevelWarn = "warning"
    showLevel LevelError = "error"
    showLevel (LevelOther x) = T.unpack x

