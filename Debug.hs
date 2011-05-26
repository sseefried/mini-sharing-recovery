module Debug (
  initTrace, queryTrace, traceLine, traceChunk, trace
) where

-- standard libraries
import Control.Monad
import Data.IORef
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import qualified Debug.Trace as Trace

-- This flag indicates whether tracing messages should be emitted.
--
traceFlag :: IORef Bool
{-# NOINLINE traceFlag #-}
traceFlag = unsafePerformIO $ newIORef False
-- traceFlag = unsafePerformIO $ newIORef True

-- |Initialise the /trace flag/, which determines whether tracing messages should be emitted.
--
initTrace :: Bool -> IO ()
initTrace = writeIORef traceFlag

-- |Read the value of the /trace flag/.
--
queryTrace :: IO Bool
queryTrace = readIORef traceFlag

-- |Emit a trace message if the /trace flag/ is set.  The first string indicates the location of
-- the message.  The second one is the message itself.  The output is formatted to be on one line.
--
traceLine :: String -> String -> IO ()
traceLine header msg
  = do { doTrace <- queryTrace
       ; when doTrace 
         $ hPutStrLn stderr (header ++ ": " ++ msg)
       }

-- |Emit a trace message if the /trace flag/ is set.  The first string indicates the location of
-- the message.  The second one is the message itself.  The output is formatted over multiple
-- lines.
--
traceChunk :: String -> String -> IO ()
traceChunk header msg
  = do { doTrace <- queryTrace
       ; when doTrace 
         $ hPutStrLn stderr (header ++ "\n  " ++ msg)
       }

-- | Like Debug.Trace but only when the /trace flag/ is set.
trace :: String -> a -> a
trace msg v = unsafePerformIO $ do
  doTrace <- queryTrace
  if doTrace
    then return $ Trace.trace msg v
    else return v