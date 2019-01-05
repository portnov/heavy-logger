{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, MultiParamTypeClasses, UndecidableInstances, RecordWildCards #-}

-- | This module contains definitions for formatting log message to write it to output.
--
-- Log message format is defined by using @text-format-heavy@ syntax. Variables available are:
--
-- * level - message severity level. Variable format can be specified in form of
--   @selector[~convert]@, where:
--
--     * @selector@ is @name@ for level name, @value@ for level integer value,
--       @syslog@ for name of syslog equivalent of the level.
--
--     * @convert@ is @u@ for upper case, @l@ for lower case, @t@ for title case
--       (all words capitalized).
--   
--     Default format corresponds to @name@. For example, use @{level:~l}@ to
--     output level name in lower case.
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
  ( LogMessageWithTime (..),
    defaultLogFormat,
    formatLogMessage,
    formatLogMessage'
  ) where

import Control.Applicative
import Control.Monad
import Data.List (intercalate)
import Data.Maybe
import Data.Monoid
import Data.Default
import qualified Data.Map as M
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Builder.Int as Builder
import System.Log.FastLogger
import qualified Data.Text.Format.Heavy as F
import qualified Data.Text.Format.Heavy.Parse as PF
import Data.Text.Format.Heavy.Formats (Conversion (..))
import Data.Text.Format.Heavy.Build (convertText)
import Data.Attoparsec.Text
import Language.Haskell.TH.Syntax (Loc (..))
import Prelude hiding (takeWhile)

import System.Log.Heavy.Types
import System.Log.Heavy.Level

data LogMessageWithTime = LogMessageWithTime FormattedTime LogMessage

data LevelFormatSelector = ShowLevelName | ShowLevelValue | ShowSyslog
  deriving (Eq, Show)

data LevelFormat = LevelFormat {
    lfSelector :: LevelFormatSelector
  , lfConvert :: Maybe Conversion
  }
  deriving (Eq, Show)

instance Default LevelFormat where
  def = LevelFormat ShowLevelName Nothing

instance F.IsVarFormat LevelFormat where
  parseVarFormat text = either (Left . show) Right $ doParse $ TL.toStrict text
    where
      pFormat :: Parser LevelFormat
      pFormat = do
        mbSelector <- optionMaybe (pSelector <?> "level detail selector")
        let selector = fromMaybe ShowLevelName mbSelector
        mbConvert <- optionMaybe (pConvert <?> "conversion specification")
        return $ LevelFormat selector mbConvert

      optionMaybe p = option Nothing (Just <$> p)

      pSelector :: Parser LevelFormatSelector
      pSelector =
        try (string "value" >> return ShowLevelValue) <|>
        try (string "syslog" >> return ShowSyslog) <|>
        (string "name" >> return ShowLevelName)

      pConvert :: Parser Conversion
      pConvert = do
        char '~'
        conv <- satisfy (`elem` ['u', 'l', 't'])
        case conv of
          'u' -> return UpperCase
          'l' -> return LowerCase
          't' -> return TitleCase

      doParse text = parseOnly pFormat text

instance F.Formatable Level where
  formatVar Nothing level = Right $ Builder.fromText (levelName level)
  formatVar (Just fmt) level = do
    lf <- F.parseVarFormat fmt
    let text = case lfSelector lf of
                 ShowLevelName -> Builder.fromText (levelName level)
                 ShowLevelValue -> Builder.decimal (levelInt level)
                 ShowSyslog -> Builder.fromString (show $ levelToPriority level)
    Right $ convertText (lfConvert lf) text

instance F.VarContainer LogMessageWithTime where
  lookupVar name (LogMessageWithTime ftime  (LogMessage {..})) =
      case lookup name stdVariables of
        Just value -> Just value
        Nothing -> Just $ fromMaybe (F.Variable TL.empty) $ msum $ map (lookup name) contextVariables
    where
      stdVariables :: [(TL.Text, F.Variable)]
      stdVariables = [("level", F.Variable lmLevel),
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
formatLogMessage fmt msg ftime = toLogStr $ formatLogMessage' fmt msg ftime

-- | Format log message for output.
formatLogMessage' :: F.Format -> LogMessage -> FormattedTime -> TL.Text
formatLogMessage' fmt msg ftime = F.format fmt $ LogMessageWithTime ftime msg

