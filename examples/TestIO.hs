{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, FlexibleContexts, TemplateHaskell #-}

import Control.Monad (forM_, replicateM)
import Control.Concurrent
import System.Environment
import System.IO
import System.Log.Heavy
import System.Log.Heavy.TH
import System.Log.Heavy.IO
import Data.Text.Format.Heavy

logFormat :: Format
logFormat = "{time} [{level:~t}] Thread #{thread}: {message}\nSource: {file} +{line}\n"

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
  withLoggingIO settings $ withLogVariable "thread" ("hello" :: String) $ do
      putStr "Your name? "
      hFlush stdout
      name <- getLine
      $info "name was {}" (Single name)
      putStrLn $ "Hello, " ++ name

testThreads :: LoggingSettings -> IO ()
testThreads settings = do
  let startIdxs = [100, 200, 300] :: [Int]
  withLoggingIO settings $ withLogVariable "thread" ("test" :: String) $ do 
      -- MVars are used for synchronization
      mvs <- replicateM (length startIdxs) newEmptyMVar
      $info "Running {} threads" (Single $ length startIdxs)
      forM_ (zip startIdxs mvs) $ \(start, mvar) -> do
         forkIO $
           withLoggingIO settings $ withLogVariable "thread" start $ do
               $info "Thread started" ()
               forM_ [start .. start + 30] $ \counter -> do
                 $info "Counter is {}" (Single counter)
                 threadDelay $ 500 * 1000
               putMVar mvar () -- mark thread as done
               $info "Thread finished" ()
      forM_ mvs takeMVar -- read for all threads to finish.
      $info "All threads finished" ()

