{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, FlexibleContexts, TemplateHaskell #-}

import Control.Monad (forM_, replicateM)
import Control.Monad.Trans
import Control.Concurrent
import System.Environment
import System.IO
import System.Log.Heavy
import System.Log.Heavy.TH
import Data.Text.Format.Heavy

logFormat :: Format
logFormat = "{time} [{level:~l}] #{thread} ({file} +{line}): {message}\n"

selectBackend :: String -> LoggingSettings
selectBackend "syslog" = LoggingSettings $ defaultSyslogSettings {ssFormat = logFormat}
selectBackend "stderr" = LoggingSettings $ defStderrSettings {lsFormat = logFormat}
selectBackend "stdout" = LoggingSettings $ defStdoutSettings {lsFormat = logFormat}
selectBackend "null" = LoggingSettings $ NullLogSettings
selectBackend "parallel" =
  LoggingSettings $ ParallelLogSettings [
                      LoggingSettings defStderrSettings {lsFormat = logFormat},
                      LoggingSettings defaultSyslogSettings {ssFormat = logFormat}
                    ]
selectBackend path = LoggingSettings $ defFileSettings path

main :: IO ()
main = do
  [bstr] <- getArgs
  let settings = selectBackend bstr
  testHello settings
  testThreads settings

testHello :: LoggingSettings -> IO ()
testHello settings = do
  withLoggingT settings $ withLogVariable "thread" ("hello" :: String) $ do
      liftIO $ putStr "Your name? "
      liftIO $ hFlush stdout
      name <- liftIO $ getLine
      $info "name was {}" (Single name)
      liftIO $ putStrLn $ "Hello, " ++ name

testThreads :: LoggingSettings -> IO ()
testThreads settings = do
  let startIdxs = [100, 200, 300] :: [Int]
  withLoggingT settings $ withLogVariable "thread" ("hello" :: String) $ do
      -- MVars are used for synchronization
      mvs <- liftIO $ replicateM (length startIdxs) newEmptyMVar
      $info "Running {} threads" (Single $ length startIdxs)
      forM_ (zip startIdxs mvs) $ \(start, mvar) -> do
         liftIO $ forkIO $
           withLoggingT settings $ withLogVariable "thread" start $ do
               $info "Thread started" ()
               forM_ [start .. start + 30] $ \counter -> do
                 $info "Counter is {}" (Single counter)
                 liftIO $ threadDelay $ 500 * 1000
               liftIO $ putMVar mvar () -- mark thread as done
               $info "Thread finished" ()
      liftIO $ forM_ mvs takeMVar -- read for all threads to finish.
      $info "All threads finished" ()

