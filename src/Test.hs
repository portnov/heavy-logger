{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving #-}

import Control.Monad.Trans
import Control.Monad.Logger (LogLevel (..))
import Data.Monoid
import System.Environment
import System.IO
import qualified System.Posix.Syslog as Syslog
import System.Log.Heavy
import System.Log.FastLogger
import Data.Text.Format.Heavy (Single (..))

selectBackend :: String -> LogBackend
selectBackend "syslog" = LogBackend $ defaultSyslogSettings
selectBackend "stderr" = LogBackend $ defStderrSettings
selectBackend "stdout" = LogBackend $ defStdoutSettings
selectBackend path = LogBackend $ defFileSettings path

main :: IO ()
main = do
  [bstr] <- getArgs
  let backend = selectBackend bstr

  withLogging backend id $ do
      liftIO $ putStr "Your name? "
      liftIO $ hFlush stdout
      name <- liftIO $ getLine
      logMessage $ infoMessage "name was {}" (Single name)
      liftIO $ putStrLn $ "Hello, " ++ name




