module Test.Pos.Util.LogSpec
    ( spec)
where

import           Universum hiding (replicate)

import           Control.Concurrent (threadDelay)

import           Data.Text (replicate)
import           Data.Text.Buildable (build)
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder (toLazyText)
import           Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import           Data.Time.Units (Microsecond, fromMicroseconds)
import           Test.Hspec (Spec, describe, it)
import           Test.Hspec.Core.QuickCheck (modifyMaxSize, modifyMaxSuccess)
import           Test.QuickCheck (Property, property)
import           Test.QuickCheck.Monadic (assert, monadic', monadicIO, run)

import           Pos.Util.Log
import           Pos.Util.Log.Internal (getLinesLogged)
import           Pos.Util.Log.LogSafe (SecureLog (..), logDebugS, logErrorS, logInfoS, logNoticeS,
                                       logWarningS)
import           Pos.Util.Log.Severity (Severity (..))
import           Pos.Util.LoggerConfig (defaultInteractiveConfiguration, defaultTestConfiguration)

import qualified Pos.Util.Log as Log
import qualified Pos.Util.Trace as Tr
import qualified Pos.Util.Trace.Klog as K
import qualified Pos.Util.Trace.Unstructured as Tu

nominalDiffTimeToMicroseconds :: POSIXTime -> Microsecond
nominalDiffTimeToMicroseconds = fromMicroseconds . round . (* 1000000)

prop_small :: Property
prop_small =
    monadicIO $ do
        (diffTime,_) <- run (run_logging Debug 1 20 10)
        assert (diffTime > 0)

prop_large :: Property
prop_large =
    monadicIO $ do
        (diffTime,_) <- run (run_logging Debug 100 200 100)
        assert (diffTime > 0)

-- | Count as many lines as you itented to log.
prop_lines :: Property
prop_lines =
    monadicIO $ do
        let n0 = 20
            n1 = 1
        (_, linesLogged) <- run (run_logging Debug 10 n0 n1)
        -- multiply by 5 because we log 5 different messages (no * n1) times
        assert (linesLogged == n0 * n1 * 5)

-- | Count as many lines as you itented to log.
prop_sev :: Property
prop_sev =
    monadicIO $ do
        let n0 = 20
            n1 = 1
        (_, linesLogged) <- run (run_logging Warning 10 n0 n1)
        -- multiply by 2 because Debug, Info and Notice messages must not be logged
        assert (linesLogged == n0 * n1 * 2)

run_logging :: Severity -> Int -> Integer -> Integer-> IO (Microsecond, Integer)
run_logging sev n n0 n1= do
        startTime <- getPOSIXTime
{- -}
        setupLogging $ defaultTestConfiguration sev
        forM_ [1..n0] $ \_ ->
            usingLoggerName "test_log" $
                forM_ [1..n1] $ \_ -> do
                    logDebug msg
                    logInfo msg
                    logNotice msg
                    logWarning msg
                    logError msg
{- -}
        endTime <- getPOSIXTime
        threadDelay 0500000
        diffTime <- return $ nominalDiffTimeToMicroseconds (endTime - startTime)
        putStrLn $ "  time for " ++ (show (n0*n1)) ++ " iterations: " ++ (show diffTime)
        linesLogged <- getLinesLogged
        putStrLn $ "  lines logged :" ++ (show linesLogged)
        return (diffTime, linesLogged)
        where msg :: Text
              msg = replicate n "abcdefghijklmnopqrstuvwxyz"
----
prop_sevS :: Property
prop_sevS =
    monadicIO $ do
        let n0 = 200
            n1 = 1
        (_, linesLogged) <- run (run_loggingS Warning 10 n0 n1)
        -- multiply by 2 because Debug, Info and Notice messages must not be logged
        assert (linesLogged == n0 * n1 * 2)

run_loggingS :: Severity -> Int -> Integer -> Integer-> IO (Microsecond, Integer)
run_loggingS sev n n0 n1= do
        startTime <- getPOSIXTime
{- -}
        setupLogging $ defaultTestConfiguration sev
        forM_ [1..n0] $ \_ ->
            usingLoggerName "test_log" $
                forM_ [1..n1] $ \_ -> do
                    logDebugS   $ toStrict $ toLazyText $ build $ SecureLog msg
                    logInfoS    $ toStrict $ toLazyText $ build $ SecureLog msg
                    logNoticeS  $ toStrict $ toLazyText $ build $ SecureLog msg
                    logWarningS $ toStrict $ toLazyText $ build $ SecureLog msg
                    logErrorS   $ toStrict $ toLazyText $ build $ SecureLog msg

{- -}
        endTime <- getPOSIXTime
        threadDelay 0500000
        diffTime <- return $ nominalDiffTimeToMicroseconds (endTime - startTime)
        putStrLn $ "  time for " ++ (show (n0*n1)) ++ " iterations: " ++ (show diffTime)
        linesLogged <- getLinesLogged
        putStrLn $ "  lines logged :" ++ (show linesLogged)
        return (diffTime, linesLogged)
        where msg :: Text
              msg = replicate n "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

prop_sevK :: Property
prop_sevK =
    monadicIO $ do
        let n0 = 200
            n1 = 1
        setupLogging (defaultInteractiveConfiguration Debug)
        Tr.traceWith K.klogTrace K.LogNamed { K.lnName = toText "NOTused"
                                            , K.lnItem = Tu.LogItem Tu.Both Warning (toText "haha")
                                            }
        assert (200 == n0 * n1)

spec :: Spec
spec = describe "Log" $ do
    modifyMaxSuccess (const 2) $ modifyMaxSize (const 2) $ it "measure time for logging small messages" $ property prop_small
    modifyMaxSuccess (const 2) $ modifyMaxSize (const 2) $ it "measure time for logging LARGE messages" $ property prop_large
    modifyMaxSuccess (const 2) $ modifyMaxSize (const 2) $ it "lines counted as logged must be equal to how many was itended to be written" $ property prop_lines
    modifyMaxSuccess (const 2) $ modifyMaxSize (const 2) $ it "Debug, Info and Notice messages must not be logged" $ property prop_sev
    modifyMaxSuccess (const 2) $ modifyMaxSize (const 2) $ it "Debug, Info and Notice messages must not be logged for safe version also" $ property prop_sevS
    modifyMaxSuccess (const 2) $ modifyMaxSize (const 2) $ it "Dummy test to see if Trace Katip are combined well." $ property prop_sevK
