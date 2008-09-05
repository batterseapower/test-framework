module Test.Framework.TimeoutIO (
        Seconds,
        timeoutIO
    ) where

import Control.Concurrent
import Control.Exception
import Control.Monad

import Data.Maybe

import Prelude hiding (catch)


-- | Documentation type that records the places where we expect to get a time span in integral numbers of seconds
type Seconds = Int

-- | Runs an action for the given number of seconds: returns the value it returns immediately if it terminates
-- within that timespan, otherwise returns @Nothing@.
timeoutIO :: Seconds -> IO a -> IO (Maybe a)
timeoutIO seconds action = do
    -- The "I have won" variable is used to ensure that at most one of the
    -- two threads attempts to put something into the result variable
    i_have_won_var <- newMVar ()
    result_var <- newEmptyMVar
    
    -- Right, spin off a thread to do the action and put it into the MVar
    thread_id <- forkIO $ do
        result <- action
        mb_i_won <- tryTakeMVar i_have_won_var
        -- If I won then just write a result: the other thread will die eventually
        when (isJust mb_i_won) $ putMVar result_var (Just result)
    
    -- Start another thread that will kill the other one after an elapsed period. As
    -- a precaution we also fill the result_var if this thread throws an exception, so
    -- timeout is never blocked forever.
    forkIO $ flip catch (\exception -> putMVar result_var Nothing >> throwIO exception) $ do
        threadDelay (seconds * 1000000)
        mb_i_won <- tryTakeMVar i_have_won_var
        -- If I won then kill off the other guy if necessary and write a null result
        when (isJust mb_i_won) $ do
            killThread thread_id
            putMVar result_var Nothing
    
    -- Extract the result from the variable: exactly one of the two threads above
    -- should have won the race and written something into the variable
    takeMVar result_var