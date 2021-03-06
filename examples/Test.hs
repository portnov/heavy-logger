{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, FlexibleContexts #-}

import Control.Monad.Trans
import System.Environment
import System.IO
import System.Log.Heavy
import System.Log.Heavy.Shortcuts
import Data.Text.Format.Heavy

logFormat :: Format
logFormat = "{time} [{level}] {appname} {source}: {message}\n"

selectBackend :: String -> LoggingSettings
selectBackend "syslog" = LoggingSettings $ defaultSyslogSettings {ssFormat = logFormat}
selectBackend "stderr" = LoggingSettings $ defStderrSettings {lsFormat = logFormat}
selectBackend "stdout" = LoggingSettings $ defStdoutSettings {lsFormat = logFormat}
selectBackend "null" = LoggingSettings $ NullLogSettings
selectBackend "parallel" =
  LoggingSettings $ ParallelLogSettings [LoggingSettings defStderrSettings, LoggingSettings defaultSyslogSettings]
selectBackend path = LoggingSettings $ defFileSettings path

main :: IO ()
main = do
  [bstr] <- getArgs
  let settings = selectBackend bstr
  let contextVariables = [("appname", Variable ("hello world" :: String))]
  withLoggingT settings $ withLogContext (LogContextFrame contextVariables (include defaultLogFilter)) $ do
      liftIO $ putStr "Your name? "
      liftIO $ hFlush stdout
      name <- liftIO $ getLine
      info "name was {}" (Single name)
      liftIO $ putStrLn $ "Hello, " ++ name




