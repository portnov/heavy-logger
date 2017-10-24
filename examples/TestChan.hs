{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, FlexibleContexts, TemplateHaskell, RecordWildCards #-}

import Control.Monad (forM_, replicateM, forever)
import Control.Monad.Trans
import Control.Concurrent
import qualified Data.Text.Lazy.IO as TLIO
import System.Log.Heavy
import System.Log.Heavy.TH
import Data.Text.Format.Heavy
import Data.Text.Format.Heavy.Parse (parseFormat')

chanLogger :: Chan LogMessage -> IO ()
chanLogger chan = forever $ do
  LogMessage {..} <- readChan chan
  TLIO.putStrLn $ format (parseFormat' lmFormatString) lmFormatVars

main :: IO ()
main = do
  chan <- newChan
  let settings = LoggingSettings (ChanLoggerSettings chan)
  forkIO $ chanLogger chan
  testThreads settings

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

