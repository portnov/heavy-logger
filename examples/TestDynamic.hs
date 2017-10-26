{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, FlexibleContexts, TemplateHaskell #-}

import Control.Monad (when, forM_)
import Control.Concurrent
import Control.Monad.Trans
import System.IO
import System.Log.Heavy
import System.Log.Heavy.TH
import Data.Text.Format.Heavy

logFormat :: Format
logFormat = "{time} [{level}] #{thread} {source}: {message}\n"

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
  putStrLn "1"
  dynamicHandle <- newDynamicBackendHandle settings
  putStrLn "2"
  let dynamicSettings = LoggingSettings (DynamicSettings dynamicHandle)
  putStrLn "3"
  withLoggingT dynamicSettings $ do
      liftIO $ putStrLn "4"
      liftIO $ forkIO $ worker dynamicSettings
      controller dynamicHandle

worker :: LoggingSettings -> IO ()
worker settings = do
  withLoggingT settings $ withLogVariable "thread" ("worker" :: String) $ do
    forM_ ([1..] :: [Integer]) $ \counter -> do
        liftIO $ putStrLn "5"
        $info "Counter is {}." (Single counter)
        liftIO $ threadDelay $ 500 * 1000

controller :: DynamicBackendHandle -> LoggingT IO ()
controller handle =
    withLogVariable "thread" ("controller" :: String) $ do
        liftIO $ putStrLn "6"
        go initBackend
  where
    go oldBackendStr = do
      liftIO $ putStrLn "7"
      $info "Current backend is {}" (Single oldBackendStr)
      liftIO $ putStr "Next backend? "
      liftIO $ hFlush stdout
      newBackendStr <- liftIO getLine
      if newBackendStr == "quit"
        then return ()
        else do
             when (newBackendStr /= oldBackendStr) $ do
               let newSettings = selectBackend newBackendStr
               liftIO $ updateDynamicBackendSettings handle newSettings
             go newBackendStr

