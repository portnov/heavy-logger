{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, MultiParamTypeClasses, UndecidableInstances, ScopedTypeVariables, FlexibleContexts #-}

module System.Log.Heavy.Util
  (
    -- * Functions of common use
    logMessage,
    -- * Utilities for backends implementation
    checkLogLevel, checkLogLevel',
    checkContextFilter, checkContextFilter', checkContextFilterM
  ) where

import Control.Monad (when)
import Control.Monad.Trans
import Data.List (isPrefixOf)

import System.Log.Heavy.Types
import System.Log.Heavy.Level

-- | Check if message source and level passes specified filters.
--
-- The message is passed if:
--
-- * No @include@ filters are defined in context stack, OR the message conforms to ANY of @include@ filters;
--
-- * AND the message does not conform to any of @exclude@ filters in the stack.
--
checkContextFilter' :: [LogContextFilter] -> LogSource -> Level -> Bool
checkContextFilter' filters source level =
  let includeFilters = [fltr | LogContextFilter (Just fltr) _ <- filters]
      excludeFilters = [fltr | LogContextFilter _ (Just fltr) <- filters]
      includeOk = null includeFilters || or [checkLogLevel' fltr source level | fltr <- includeFilters]
      excludeOk = or [checkLogLevel' fltr source level | fltr <- excludeFilters]
  in  includeOk && not excludeOk

-- | Check if message matches filters from logging context.
--
-- The message is passed if:
--
-- * No @include@ filters are defined in context stack, OR the message conforms to ANY of @include@ filters;
--
-- * AND the message does not conform to any of @exclude@ filters in the stack.
--
checkContextFilter :: LogContext -> LogMessage -> Bool
checkContextFilter context msg =
  checkContextFilter' (map lcfFilter context) (lmSource msg) (lmLevel msg)

-- | Check if message matches filters from logging context.
-- This function is similar to @checkContextFilter@, but uses current context
-- from monadic state.
checkContextFilterM :: HasLogContext m => LogMessage -> m Bool
checkContextFilterM msg = do
  context <- getLogContext
  return $ checkContextFilter context msg

-- | Check if message level matches given filter.
checkLogLevel :: LogFilter -> LogMessage -> Bool
checkLogLevel fltr m =
    checkLogLevel' fltr (lmSource m) (lmLevel m)

-- | Check if message level matches given filter.
checkLogLevel' :: LogFilter -> LogSource -> Level -> Bool
checkLogLevel' fltr source level =
    case lookup (bestMatch source (map fst fltr)) fltr of
      Nothing -> False
      Just min -> level <= min
  where
    bestMatch :: LogSource -> [LogSource] -> LogSource
    bestMatch src list = go [] src list

    go :: LogSource -> LogSource -> [LogSource] -> LogSource
    go best src [] = best
    go best src (x:xs)
      | src == x = x
      | (x `isPrefixOf` src) && (length x > length best) = go x src xs
      | otherwise = go best src xs

-- | Log a message. This will add current context to context specified
-- in the message.
-- This function checks current context filter.
logMessage :: forall m. (HasLogging m, MonadIO m) => LogMessage -> m ()
logMessage msg = do
  ok <- checkContextFilterM msg
  when ok $ do
    context <- getLogContext
    logger <- getLogger
    liftIO $ logger $ msg {lmContext = context ++ lmContext msg}

