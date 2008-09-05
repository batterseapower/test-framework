module Test.Framework.TimeoutIO (
        Seconds,
        timeoutIO
    ) where

import Control.Concurrent
import Control.Exception

import Prelude hiding (catch)


-- | Documentation type that records the places where we expect to get a time span in integral numbers of seconds
type Seconds = Int

-- | Runs an action for the given number of seconds: returns the value it returns immediately if it terminates
-- within that timespan, otherwise returns @Nothing@.
timeoutIO :: Seconds -> IO a -> IO (Maybe a)
timeoutIO seconds action = do
    -- We create two threads which compete to write into this variable first,
    -- and that is the value we return from this function
    result_var <- newEmptyMVar
    
    -- Right, spin off a thread to do the action and put it into the MVar
    thread_id <- forkIO $ do
        result <- action
        -- Just try and write a result: the other thread will die eventually
        tryPutMVar result_var (Just result)
        return ()
    
    -- Start another thread that will kill the other one after an elapsed period. As
    -- a precaution we also fill the result_var if this thread throws an exception, so
    -- timeout is never blocked forever.
    forkIO $ flip catch (\exception -> putMVar result_var Nothing >> throwIO exception) $ do
        threadDelay (seconds * 1000000)
        -- Kill off the other guy (has no effect if already dead) and write a null result
        killThread thread_id
        tryPutMVar result_var Nothing
        return ()
    
    -- Extract the result from the variable: at least one of the two threads above
    -- should win the race and write something into the variable
    takeMVar result_var