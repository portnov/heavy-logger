{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, MultiParamTypeClasses, UndecidableInstances, RecordWildCards #-}

module System.Log.Heavy.Format
  ( FormatItem (..),
    LogFormat,
    defaultLogFormat,
    formatLogMessage,
    parseFormat, parseFormat'
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Logger (MonadLogger (..), LogLevel (..))
import Data.List (intersperse, intercalate)
import Data.String
import Data.Char
import Data.Maybe
import qualified Data.Text as T
import qualified Data.ByteString as B
import Data.Attoparsec.ByteString
import System.Log.FastLogger
import qualified Data.Text.Format.Heavy as F
import qualified Data.Text.Format.Heavy.Parse as PF
import Prelude hiding (takeWhile)

import System.Log.Heavy.Types

-- | Log message formatting item
data FormatItem =
    FLevel               -- ^ Log level
  | FSource              -- ^ Log message source (module)
  | FLocation            -- ^ Log message source (full location)
  | FTime                -- ^ Log message time
  | FMessage             -- ^ Log message itself
  | FString B.ByteString -- ^ Fixed string, e.g. some kind of separator
  deriving (Eq, Show)

-- instance IsString FormatItem where
--   fromString str = FString $ fromString str

-- | Log message format description
newtype LogFormat = LogFormat [FormatItem]

instance IsString LogFormat where
  fromString str = parseFormat' (fromString str)

-- | Default log message format.
-- Corresponds to: @$time [$level] $source: $message\n@
defaultLogFormat :: LogFormat
defaultLogFormat = parseFormat' "$time [$level] $source: $message\n"

formatLogMessage :: LogFormat -> LogMessage -> FormattedTime -> LogStr
formatLogMessage (LogFormat format) (LogMessage {..}) ftime = mconcat $ map go format
  where
    go :: FormatItem -> LogStr
    go FLevel = toLogStr $ showLevel lmLevel
    go FSource = toLogStr $ intercalate "." lmSource
    go FLocation = toLogStr $ show lmLocation
    go FTime = toLogStr ftime
    go FMessage =
      let fmt = PF.parseFormat' lmFormatString
      in  toLogStr $ F.format fmt lmFormatVars
    go (FString s) = toLogStr s

    showLevel LevelDebug = "debug"
    showLevel LevelInfo = "info"
    showLevel LevelWarn = "warning"
    showLevel LevelError = "error"
    showLevel (LevelOther x) = T.unpack x

-- | Parse log message format description string.
-- Variables substitution in @${}@-style is supported.
-- Only following variables can be used: 
-- level, source, location, time, message.
parseFormat :: B.ByteString -> Either String LogFormat
parseFormat formatstr = parseOnly (pFormat <* endOfInput) formatstr

-- | Version of parseFormat which throws an error if parsing failed.
parseFormat' :: B.ByteString -> LogFormat
parseFormat' formatstr =
  case parseFormat formatstr of
    Right format -> format
    Left err -> error err

pFormat :: Parser LogFormat
pFormat = LogFormat <$> many1 pItem

pItem :: Parser FormatItem
pItem = choice [
          item "level" FLevel,
          item "source" FSource,
          item "location" FLocation,
          item "time" FTime,
          item "message" FMessage,
          fixed ]
  where
    item :: String -> FormatItem -> Parser FormatItem
    item str item = (do
      string "$"
      next <- optional $ string "$"
      case next of
        Just _ -> return $ FString "$"
        Nothing -> do
          mbBrace <- optional $ string "{"
          string (fromString str)
          when (isJust mbBrace) $ do
            string "}"
            return ()
          return item) <?> str

    fixed :: Parser FormatItem
    fixed = FString <$> takeWhile1 (\w -> chr (fromIntegral w) /= '$')

