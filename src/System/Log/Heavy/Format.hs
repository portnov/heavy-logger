{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, MultiParamTypeClasses, UndecidableInstances, RecordWildCards #-}

module System.Log.Heavy.Format
  ( defaultLogFormat,
    formatLogMessage
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Logger (MonadLogger (..), LogLevel (..))
import Data.List (intersperse, intercalate)
import Data.String
import Data.Char
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as B
-- import Data.Attoparsec.ByteString
import System.Log.FastLogger
import qualified Data.Text.Format.Heavy as F
import qualified Data.Text.Format.Heavy.Parse as PF
import Prelude hiding (takeWhile)

import System.Log.Heavy.Types

-- | Default log message format.
-- Corresponds to: @$time [$level] $source: $message\n@
defaultLogFormat :: F.Format
defaultLogFormat = PF.parseFormat' "{time} [{level}] {source}: {message}\n"

formatLogMessage :: F.Format -> LogMessage -> FormattedTime -> LogStr
formatLogMessage fmt (LogMessage {..}) ftime =
    toLogStr $ F.format fmt variables
  where
    variables :: [(TL.Text, F.Variable)]
    variables =  [("level", F.Variable $ showLevel lmLevel),
                  ("source", F.Variable $ intercalate "." lmSource),
                  ("location", F.Variable $ show lmLocation),
                  ("time", F.Variable ftime),
                  ("message", F.Variable formattedMessage)]

    formattedMessage =
      let fmt = PF.parseFormat' lmFormatString
      in  F.format fmt lmFormatVars

    showLevel LevelDebug = "debug"
    showLevel LevelInfo = "info"
    showLevel LevelWarn = "warning"
    showLevel LevelError = "error"
    showLevel (LevelOther x) = T.unpack x

