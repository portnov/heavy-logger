{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, FlexibleContexts #-}

import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Logger (LogLevel (..))
import Data.Monoid
import System.Environment
import System.IO
import qualified System.Posix.Syslog as Syslog
import System.Log.Heavy
import System.Log.Heavy.Shortcuts
import System.Log.FastLogger
import Data.Text.Format.Heavy (Single (..))

selectBackend :: String -> LoggingSettings
selectBackend "syslog" = LoggingSettings $ defaultSyslogSettings
selectBackend "stderr" = LoggingSettings $ defStderrSettings
selectBackend "stdout" = LoggingSettings $ defStdoutSettings
selectBackend path = LoggingSettings $ defFileSettings path

main :: IO ()
main = do
  [bstr] <- getArgs
  let settings = selectBackend bstr
  runReaderT (main' settings) undefined


-- main' :: IsLogBackend b => LoggingSettings -> ReaderT b IO ()
main' settings = do
  withLogging settings $ do
      liftIO $ putStr "Your name? "
      liftIO $ hFlush stdout
      name <- liftIO $ getLine
      info "name was {}" (Single name)
      liftIO $ putStrLn $ "Hello, " ++ name




