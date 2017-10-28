{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright   : (c) 2013-2016 Brendan Hay, 2017 Ilya Portnov
-- License     : Mozilla Public License, v. 2.0.
-- Portability : non-portable (GHC extensions)
--
module Main where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.AWS
import           Data.ByteString.Builder (hPutBuilder)
import           Data.Conduit
import qualified Data.Conduit.List       as CL
import           Data.Monoid
import           Network.AWS.Data
import           Network.AWS.EC2
import           System.IO
import           System.Log.Heavy
import           System.Log.Heavy.IO
import           System.Log.Heavy.AWS

instanceOverview :: Region -> IO ()
instanceOverview r = withLoggingIO (LoggingSettings defStderrSettings) $ do
    lgr <- getAwsLogger
    let credentials = FromFile "batchd" "credentials"
    env <- newEnv credentials <&> set envLogger lgr

    let pp x = mconcat
          [ "[instance:" <> build (x ^. insInstanceId) <> "] {"
          , "\n  public-dns = " <> build (x ^. insPublicDNSName)
          , "\n  tags       = " <> build (x ^. insTags . to show)
          , "\n  state      = " <> build (x ^. insState . isName . to toBS)
          , "\n}\n"
          ]

    runResourceT . runAWST env . within r $
        paginate describeInstances
            =$= CL.concatMap (view dirsReservations)
            =$= CL.concatMap (view rInstances)
             $$ CL.mapM_ (liftIO . hPutBuilder stdout . pp)

main :: IO ()
main = do
  instanceOverview Oregon

