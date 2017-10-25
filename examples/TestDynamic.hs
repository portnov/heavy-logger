{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, FlexibleContexts, TemplateHaskell #-}

import Control.Monad (when)
import Control.Concurrent
import Control.Monad.Trans
import System.IO
import System.Log.Heavy
import System.Log.Heavy.TH
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
selectBackend path = error "unsupported"

initBackend :: String
initBackend = "stderr"

main :: IO ()
main = do
  let settings = selectBackend initBackend
  settingsVar <- newMVar settings
  let dynamicSettings = DynamicSettings settingsVar
  withLoggingT (LoggingSettings dynamicSettings) $ do
      testDynamic settingsVar

testDynamic :: MVar LoggingSettings -> LoggingT IO ()
testDynamic settingsVar = go initBackend
  where
    go oldBackendStr = do
      $info "Current backend is {}" (Single oldBackendStr)
      liftIO $ putStr "Next backend? "
      liftIO $ hFlush stdout
      newBackendStr <- liftIO getLine
      if newBackendStr == "quit"
        then return ()
        else do
             when (newBackendStr /= oldBackendStr) $ do
               let newSettings = selectBackend newBackendStr
               liftIO $ putMVar settingsVar newSettings
             go newBackendStr

