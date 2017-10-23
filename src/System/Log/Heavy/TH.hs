{-# LANGUAGE TemplateHaskell #-}
-- | This module contains TemplateHaskell macros, that can be useful in many applications.
-- In more complex applications, however, you probably will have to write your own adapter macros,
-- which better suits your needs.
module System.Log.Heavy.TH
  (putMessage,
   trace, debug, info, warning, reportError, fatal,
   here, thisModule
  ) where

import Control.Monad.Logger (liftLoc)
-- import Language.Haskell.TH
import Language.Haskell.TH.Syntax hiding (reportError)
import Language.Haskell.TH.Lift
import Instances.TH.Lift
import qualified System.Posix.Syslog as Syslog

import System.Log.Heavy.Types
import System.Log.Heavy.Level

deriveLift ''Syslog.Priority
deriveLift ''Level

-- | TH macro to put message to log. Usage:
--
-- @
-- \$putMessage info_level "Received message #{}" (Single messageId)
-- @
--
-- Use @()@ as last argument if you do not have variables.
--
-- This code will work in any monad that implements @HasLogging@ constraint.
--
putMessage :: Level -> Q Exp
putMessage level = [| \msg vars -> do
  let loc = $(qLocation >>= liftLoc)
      src = splitDots (loc_module loc)
      message = LogMessage $(lift level) src loc msg vars []
  logMessage message
  |]

-- | TH macro to obtain current location in haskell source file.
-- Can be useful to construct @LogMessage@ structures 'by hand'.
here :: Q Exp
here = qLocation >>= liftLoc

-- | TH macro to obtain current module name in form of @LogSource@.
-- Can be useful to construct @LogMessage@ structures 'by hand'.
thisModule :: Q Exp
thisModule = do
  loc <- qLocation
  lift $ splitDots (loc_module loc)

-- | TH macro to log a message with TRACE level. Usage:
--
-- @
-- \$trace "hello, {}!" (Single name)
-- @
--
trace :: Q Exp
trace = putMessage trace_level

-- | TH macro to log a message with DEBUG level. Usage:
--
-- @
-- \$debug "hello, {}!" (Single name)
-- @
--
debug :: Q Exp
debug = putMessage debug_level

-- | TH macro to log a message with INFO level. Usage:
--
-- @
-- \$info "hello, {}!" (Single name)
-- @
--
info :: Q Exp
info = putMessage info_level

-- | TH macro to log a message with WARN level. Usage:
--
-- @
-- \$warning "Beware the {}!" (Single name)
-- @
--
warning :: Q Exp
warning = putMessage warn_level

-- | TH macro to log a message with ERROR level. Usage:
--
-- @
-- \$reportError "Transaction #{} was declined." (Single transactionId)
-- @
--
reportError :: Q Exp
reportError = putMessage error_level

-- | TH macro to log a message with FATAL level. Usage:
--
-- @
-- \$fatal "Cannot establish database connection" ()
-- @
--
fatal :: Q Exp
fatal = putMessage fatal_level

