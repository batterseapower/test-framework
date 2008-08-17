module Test.Framework.ThreadPool (
        executeOnPool
    ) where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad


executeOnPool :: Int    -- ^ Number of threads to use
              -> [IO a] -- ^ Actions to execute, left to right as far possible
              -> IO [a] -- ^ Ordered results of executing the given IO actions in parallel
executeOnPool n actions = do
    -- Prepare the channels
    input_chan <- newChan
    output_chan <- newChan
    
    -- Write Just the actions to the channel followed by one Nothing per thread
    -- that indicates they should terminate. We do this on another thread for
    -- maximum laziness (in case one the actions we are going to run depend on the
    -- output from previous actions..)
    forkIO $ writeList2Chan input_chan (map Just actions ++ replicate n Nothing)
    
    -- Spawn workers
    forM_ [1..n] (const $ forkIO $ poolWorker input_chan output_chan)
    
    -- Get the results and force the spine so we know they have all arrived
    -- and hence that all the threads we spawned above are going to terminate
    result <- getChanContents output_chan
    --evaluate $ length result
    return result

poolWorker :: Chan (Maybe (IO a)) -> Chan a -> IO ()
poolWorker input_chan output_chan = do
    -- Read an action and work out whether we should continue or stop
    mb_action <- readChan input_chan
    case mb_action of
        Nothing -> return () -- Must have run out of real actions to execute
        Just action -> do
            -- Do the action then loop
            result <- action
            writeChan output_chan result
            poolWorker input_chan output_chan