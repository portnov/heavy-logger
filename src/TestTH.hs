{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, FlexibleContexts, TemplateHaskell #-}

import Control.Monad.Trans
import Control.Monad.Reader
import Data.Monoid
import System.Environment
import System.IO
import qualified System.Posix.Syslog as Syslog
import System.Log.Heavy
import System.Log.Heavy.TH
import System.Log.FastLogger
import Data.Text.Format.Heavy
import qualified Data.Text.Format.Heavy.Parse as PF

logFormat :: Format
logFormat = PF.parseFormat' "{time} [{level}] {appname} {location}: {message}\n"

selectBackend :: String -> LoggingSettings
selectBackend "syslog" = LoggingSettings $ defaultSyslogSettings {ssFormat = logFormat}
selectBackend "stderr" = LoggingSettings $ defStderrSettings {lsFormat = logFormat}
selectBackend "stdout" = LoggingSettings $ defStdoutSettings {lsFormat = logFormat}
selectBackend "parallel" =
  LoggingSettings $ ParallelLogSettings [LoggingSettings defStderrSettings, LoggingSettings defaultSyslogSettings]
selectBackend path = LoggingSettings $ defFileSettings path

main :: IO ()
main = do
  [bstr] <- getArgs
  let settings = selectBackend bstr
  let contextVariables = [("appname", Variable ("hello world" :: String))]
  withLoggingT settings $ withLogContext (LogContextFrame contextVariables (Include defaultLogFilter)) $ do
      liftIO $ putStr "Your name? "
      liftIO $ hFlush stdout
      name <- liftIO $ getLine
      $info "name was {}" (Single name)
      liftIO $ putStrLn $ "Hello, " ++ name





