-- | This module provides functions to integrate @amazonka@ logging with 
-- @heavy-logging@ package.
module System.Log.Heavy.AWS
  (awsLogLevelToLevel,
   getAwsLogger
  ) where

import Data.Binary.Builder
import qualified Data.Text.Lazy.Encoding as TLE
import Language.Haskell.TH.Syntax (Loc (..))
import System.Log.Heavy as H
import Network.AWS.Types as AWS

-- | Translate Amazonka's LogLevel to heavy-logger's Level.
awsLogLevelToLevel :: LogLevel -> Level
awsLogLevelToLevel Info = info_level
awsLogLevelToLevel Error = error_level
awsLogLevelToLevel Debug = debug_level
awsLogLevelToLevel Trace = trace_level

-- | Obtain Amazonka's Logger in a monad that has heavy-logger's Logger.
getAwsLogger :: (Monad m, HasLogger m) => m AWS.Logger
getAwsLogger = do
    logger <- getLogger
    return $ \lvl builder -> logger (mkMessage lvl builder)
  where
    mkMessage lvl builder =
        LogMessage {
          lmLevel = awsLogLevelToLevel lvl,
          lmSource = ["Amazon"],
          lmLocation = Loc "Unknown" "unknown" "Unknown" (0,0) (0,0),
          lmFormatString = TLE.decodeUtf8 $ toLazyByteString builder,
          lmFormatVars = (),
          lmContext = []
        }

