-- | 'Trace' backed by katip for unstructured logging.

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Util.Trace.Klog
    ( LogNamed (..)
    , LoggerName
    , klogTrace
    , modifyName
    , appendName
    , setName
    , named
    ) where

import           Data.Functor.Contravariant (Op (..), contramap)
import           Pos.Util.Log (LogContext, LoggerName)
import           Pos.Util.Log.LogSafe (logMCond)
import           Pos.Util.LoggerConfig (LogSafety (..))
import           Pos.Util.Trace (Trace (..))
import           Pos.Util.Trace.Unstructured (LogItem (..), LogPrivacy (..))
import           Universum

-- | Attach a 'LoggerName' to something.
data LogNamed item = LogNamed
    { lnName :: LoggerName
    , lnItem :: item
    }

modifyName
    :: (LoggerName -> LoggerName)
    -> Trace m (LogNamed i)
    -> Trace m (LogNamed i)
modifyName k = contramap f
  where
    f (LogNamed name item) = LogNamed (k name) item

appendName :: LoggerName -> Trace m (LogNamed i) -> Trace m (LogNamed i)
appendName lname = modifyName (<> lname)

setName :: LoggerName -> Trace m (LogNamed i) -> Trace m (LogNamed i)
setName name = modifyName (const name)

-- | Use a 'LogNamed'. A typical usage pattern is
--
--     named . appendName "world" . appendName "hello" $ wlogTrace
--       :: Trace IO LogItem
--
--   which will use the "hello"."world" logger name.
--
named :: Trace m (LogNamed i) -> Trace m i
named = contramap (LogNamed mempty)

-- FIXME needs exporting of 'logMCond' and 'LogHandlerTag' from
-- Pos.Util.Log or rewriting wlogTrace according to Katip API.
-- This removal breaks only 'lib/src/Pos/Launcher/Runner.hs'.
-- | A general log-warper-backed 'Trace', which allows for logging to public,
-- private, or both, and the choice of a 'LoggerName'.
-- NB: log-warper uses global shared mutable state. You have to initialize it
-- or else 'wlogTrace' won't do anything.
klogTrace :: LogContext m => Trace m (LogNamed LogItem)
klogTrace = Trace $ Op $ \namedLogItem ->
    let privacy = liPrivacy (lnItem namedLogItem)
        -- loggerName = lnName namedLogItem
        severity = liSeverity (lnItem namedLogItem)
        message = liMessage (lnItem namedLogItem)
     in case privacy of
            Private -> logMCond {-loggerName-} severity message selectPrivateLogs
            Public  -> logMCond {-loggerName-} severity message selectPublicLogs
            Both    -> logMCond {-loggerName-} severity message selectBoth
  where
    selectPrivateLogs :: LogSafety -> Bool
    selectPrivateLogs = not . selectPublicLogs
    selectPublicLogs :: LogSafety -> Bool
    selectPublicLogs PublicLog  = True
    selectPublicLogs PrivateLog = False
    selectBoth :: LogSafety -> Bool
    selectBoth = const True
