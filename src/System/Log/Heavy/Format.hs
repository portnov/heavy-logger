{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, MultiParamTypeClasses, UndecidableInstances, RecordWildCards #-}

-- | This module contains definitions for formatting log message to write it to output.
--
-- Log message format is defined by using @text-format-heavy@ syntax. Variables available are:
--
-- * level - message severity level
--
-- * source - message source (module name)
--
-- * location - location from where message was logged (line in source file)
--
-- * time - message time
--
-- * message - message string itself
--
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

data LogMessageWithTime = LogMessageWithTime FormattedTime LogMessage

instance F.VarContainer LogMessageWithTime where
  lookupVar name (LogMessageWithTime ftime  (LogMessage {..})) =
      case lookup name stdVariables of
        Just value -> Just value
        Nothing -> Just $ fromMaybe (F.Variable TL.empty) $ msum $ map (lookup name) contextVariables
    where
      stdVariables :: [(TL.Text, F.Variable)]
      stdVariables = [("level", F.Variable $ showLevel lmLevel),
                      ("source", F.Variable $ intercalate "." lmSource),
                      ("location", F.Variable $ show lmLocation),
                      ("time", F.Variable ftime),
                      ("message", F.Variable formattedMessage),
                      ("fullcontext", F.Variable $ show lmContext)]

      showLevel LevelDebug = "debug"
      showLevel LevelInfo = "info"
      showLevel LevelWarn = "warning"
      showLevel LevelError = "error"
      showLevel (LevelOther x) = T.unpack x

      contextVariables :: [[(TL.Text, F.Variable)]]
      contextVariables = map lcfVariables lmContext

      formattedMessage =
        let fmt = PF.parseFormat' lmFormatString
        in  F.format fmt lmFormatVars

-- | Default log message format.
-- Corresponds to: @{time} [{level}] {source}: {message}\\n@
defaultLogFormat :: F.Format
defaultLogFormat = PF.parseFormat' "{time} [{level}] {source}: {message}\n"

-- | Format log message for output.
formatLogMessage :: F.Format -> LogMessage -> FormattedTime -> LogStr
formatLogMessage fmt msg ftime = toLogStr $ F.format fmt $ LogMessageWithTime ftime msg

