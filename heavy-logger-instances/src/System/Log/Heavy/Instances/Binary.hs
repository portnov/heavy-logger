{-# LANGUAGE RecordWildCards #-}
-- | This module contains instances of Binary type class for
-- data types defined in the heavy-logger package.
--
module System.Log.Heavy.Instances.Binary where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Binary
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B
import Data.Text.Format.Heavy
import System.Log.Heavy
import System.Posix.Syslog
import Language.Haskell.TH.Syntax (Loc (..))

allVariables :: ClosedVarContainer c => c -> [(TL.Text, TL.Text)]
allVariables c = mapMaybe go (allVarNames c)
  where
    go name =
      case lookupVar name c of
        Nothing -> Nothing
        Just var -> case formatAnyVar Nothing var of
                      Left _ -> Nothing
                      Right val -> Just (name, B.toLazyText val)

getTextVariables :: Get [(TL.Text, TL.Text)]
getTextVariables = get

instance Binary Priority where
  put p = put (fromEnum p)

  get = toEnum <$> get

instance Binary Level where
  put l = do
    put (levelName l)
    put (levelInt l)
    put (levelToPriority l)

  get = Level
    <$> get
    <*> get
    <*> get

instance Binary Loc where
  put l = do
    put (loc_filename l)
    put (loc_package l)
    put (loc_module l)
    put (loc_start l)
    put (loc_end l)

  get = Loc
    <$> get
    <*> get
    <*> get
    <*> get
    <*> get

instance Binary LogContextFilter where
  put f = do
    put (setInclude f)
    put (setExclude f)

  get = LogContextFilter
    <$> get
    <*> get

-- | Serialize Variable with default format
putVariableNoformat :: Variable -> Put 
putVariableNoformat var =
  case formatAnyVar Nothing var of
    Left err -> fail err
    Right val -> put (B.toLazyText val)

-- | Deserialize Variable with default format
getVariableNoformat :: Get Variable
getVariableNoformat = do
  text <- get
  return $ Variable (text :: TL.Text)

instance Binary LogContextFrame where
  put f  = do
    put (length $ lcfVariables f)
    forM_ (lcfVariables f) $ \(name, var) -> do
      put name
      putVariableNoformat var
    put (lcfFilter f)

  get = do
    n <- get :: Get Int
    vars <- replicateM n $ do
              name <- get
              val <- getVariableNoformat
              return (name, val)
    fltr <- get
    return (LogContextFrame vars fltr)

-- | Please note: this implementation is limited:
--
--  * It stores Variables as their Text representation
--
--  * It does not take care of correct storing/restoring variable formats
--
--  * When deserializing, it always uses @[(Text, Text)]@ as variables container.
--
instance Binary LogMessage where
  put (LogMessage {..}) = do
    put lmLevel
    put lmSource 
    put lmLocation 
    put lmFormatString 
    put (allVariables lmFormatVars)
    put lmContext

  get = LogMessage
    <$> get
    <*> get
    <*> get
    <*> get
    <*> getTextVariables
    <*> get

