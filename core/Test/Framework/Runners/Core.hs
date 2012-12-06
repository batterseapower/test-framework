module Test.Framework.Runners.Core (
        RunTest(..), RunningTest, SomeImproving(..), FinishedTest, runTests,
    ) where

import Test.Framework.Core
import Test.Framework.Improving
import Test.Framework.Options
import Test.Framework.Runners.Options
import Test.Framework.Runners.TestPattern
import Test.Framework.Runners.ThreadPool
import Test.Framework.Seed
import Test.Framework.Utilities

import Control.Concurrent.MVar
import Control.Exception (mask, finally, onException)
import Control.Monad
import Data.Maybe
import Data.Monoid


-- | A test that has been executed or is in the process of execution
data RunTest a = RunTest TestName TestTypeName a
               | RunTestGroup TestName [RunTest a]
               deriving (Show)

data SomeImproving = forall i r. TestResultlike i r => SomeImproving (i :~> r)
type RunningTest = RunTest SomeImproving

type FinishedTest = RunTest (String, Bool)

runTests :: CompleteRunnerOptions -- ^ Top-level runner options
         -> [Test]                -- ^ Tests to run
         -> IO [RunningTest]
runTests ropts tests = do
    let test_patterns = unK $ ropt_test_patterns ropts
        use_test path name = null test_patterns || any (`testPatternMatches` (path ++ [name])) test_patterns
    (run_tests, actions) <- runTests' use_test [] (unK $ ropt_test_options ropts) tests
    _ <- executeOnPool (unK $ ropt_threads ropts) actions
    return run_tests


runTest' :: ([String] -> String -> Bool) -> [String]
         -> TestOptions -> Test -> IO (Maybe (RunningTest, [IO ()]))
runTest' use_test path topts (Test name testlike)
  | use_test path name = do
    (result, action) <- runTest (completeTestOptions topts) testlike
    return (Just (RunTest name (testTypeName testlike) (SomeImproving result), [action]))
  | otherwise = return Nothing
runTest' use_test path topts (TestGroup name tests) = do
    (results, actions) <- runTests' use_test (path ++ [name]) topts tests
    return $ if null results then Nothing else Just ((RunTestGroup name results), actions)
runTest' use_test path topts (PlusTestOptions extra_topts test) = runTest' use_test path (topts `mappend` extra_topts) test
runTest' use_test path topts (BuildTestBracketed build) = mask $ \restore -> build >>= \(test, cleanup) -> do
    mb_res <- restore (runTest' use_test path topts test) `onException` cleanup
    case mb_res of
      -- No sub-tests: perform the cleanup NOW
      Nothing                  -> cleanup >> return Nothing
      Just (run_test, actions) -> do
        -- Sub-tests: perform the cleanup as soon as each of them have completed
        (mvars, actions') <- liftM unzip $ forM actions $ \action -> do
          mvar <- newEmptyMVar
          return (mvar, action `finally` putMVar mvar ())
        -- NB: the takeMVar action MUST be last in the list because the returned actions are
        -- scheduled left-to-right, and we want all the actions we depend on to be scheduled
        -- before we wait for them to complete, or we might deadlock.
        --
        -- FIXME: this is a bit of a hack because it uses one pool thread just waiting
        -- for some other pool threads to complete! Switch to parallel-io?
        return $ Just (run_test, actions' ++ [(cleanup >> mapM_ takeMVar mvars)])

runTests' :: ([String] -> String -> Bool) -> [String]
          -> TestOptions -> [Test] -> IO ([RunningTest], [IO ()])
runTests' use_test path topts = fmap (onRight concat . unzip . catMaybes) . mapM (runTest' use_test path topts)


completeTestOptions :: TestOptions -> CompleteTestOptions
completeTestOptions to = TestOptions {
            topt_seed = K $ topt_seed to `orElse` RandomSeed,
            topt_maximum_generated_tests = K $ topt_maximum_generated_tests to `orElse` 100,
            topt_maximum_unsuitable_generated_tests = K $ topt_maximum_unsuitable_generated_tests to `orElse` 1000,
            topt_maximum_test_size = K $ topt_maximum_test_size to `orElse` 100,
            topt_maximum_test_depth = K $ topt_maximum_test_depth to `orElse` 5,
            topt_timeout = K $ topt_timeout to `orElse` Nothing
        }
