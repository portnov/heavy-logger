{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Binary
import qualified Data.Text.Lazy as TL
import Data.Text.Format.Heavy
import System.Log.Heavy
import System.Log.Heavy.Instances.Binary
import Language.Haskell.TH.Syntax (Loc (..))

testMessage :: LogMessage
testMessage = LogMessage {
  lmLevel = debug_level,
  lmSource = [],
  lmLocation = Loc "<unknown>" "<unknown>" "<unknown>" (0,0) (0,0),
  lmFormatString = "hello, {}!",
  lmFormatVars = Single ("world" :: TL.Text),
  lmContext = []
}

printMessage :: LogMessage -> IO ()
printMessage (LogMessage {..}) = do
  print lmLevel
  print lmSource
  print lmLocation
  print lmFormatString
  -- print lmFormatVars
  --print (lmFormatVars :: [(TL.Text, TL.Text)])
  print lmContext

main = do
  let str = encode testMessage
  printMessage (decode str)

