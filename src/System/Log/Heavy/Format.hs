{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, MultiParamTypeClasses, UndecidableInstances, RecordWildCards #-}

-- | This module contains definitions for formatting log message to write it to output.
--
-- Log message format is defined by using @text-format-heavy@ syntax. Variables available are:
--
-- * level - message severity level
--
-- * source - message source (module name)
--
-- * location - location from where message was logged (in form of @(line, column)@).
--
-- * line - source file line number from where message was logged.
--
-- * file - source file name from where message was logged.
--
-- * package - name of the package from where message was logged.
--
-- * time - message time
--
-- * message - message string itself
--
-- * fullcontext - full set of current context variable values, in @name=value; name=value;@ form.
--
-- * Also all variables from context are available.
--
module System.Log.Heavy.Format
  ( defaultLogFormat,
    formatLogMessage
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Logger (MonadLogger (..), LogLevel (..), Loc (..))
import Data.List (intersperse, intercalate)
import Data.String
import Data.Char
import Data.Maybe
import Data.Monoid
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.ByteString as B
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
      stdVariables = [("level", F.Variable $ F.Shown lmLevel),
                      ("source", F.Variable $ intercalate "." lmSource),
                      ("location", F.Variable $ show $ loc_start lmLocation),
                      ("line", F.Variable $ fst $ loc_start lmLocation),
                      ("file", F.Variable $ loc_filename lmLocation),
                      ("package", F.Variable $ loc_package lmLocation),
                      ("time", F.Variable ftime),
                      ("message", F.Variable formattedMessage),
                      ("fullcontext", F.Variable fullContext)]

      contextVariables :: [[(TL.Text, F.Variable)]]
      contextVariables = map lcfVariables lmContext

      fullContext :: TL.Text
      fullContext = TL.concat $ map showContextVar $ M.assocs $ M.fromList $ concat contextVariables

      showContextVar :: (TL.Text, F.Variable) -> TL.Text
      showContextVar (name, value) = name <> "=" <> formatVar value <> "; "

      formatVar :: F.Variable -> TL.Text
      formatVar var = either error Builder.toLazyText $ F.formatVar Nothing var

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

