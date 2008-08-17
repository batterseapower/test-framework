module Test.Framework.Runners.Core (
        RunTest(..), runTests
    ) where

import Test.Framework.Core
import Test.Framework.Options
import Test.Framework.QuickCheck
import Test.Framework.ThreadPool

import Control.Monad

import Data.List


-- | Currently the only kind of tests result we are interested in are those properties
type TestResult = PropertyResult

-- | This is a test in-between being a 'Test' and 'RunTest'
type PendingTest = CompleteTestOptions -> IO TestResult

-- | A test that has been executed
data RunTest = RunProperty TestName PropertyResult
             | RunTestGroup TestName [RunTest]

runTests :: Int                  -- ^ Number of threads to use to execute the tests
          -> CompleteTestOptions -- ^ Top-level test options
          -> [Test]              -- ^ Tests to run
          -> IO [RunTest]
runTests threads topts tests = mdo
    -- Clever trickiness is used here to linearise the tree structure of Test such that we can run
    -- it on the thread pool.  The runTest' and runTests' functions walk through the tree, consuming
    -- one item from the result list and outputing one item into the pending-test list for every test
    -- they come across.  This function ties the knot by executing the pending tests and feeding that
    -- back as the list of completed test results.
    let (run_tests, _, requested_runs) = runTests' tests run_results
    run_results <- executeOnPool threads (map ($ topts) requested_runs)
    return run_tests

runTest' :: Test -> [TestResult] -> (RunTest, [TestResult], [PendingTest])
runTest' (Property name testable) (result:rest) = (RunProperty name result, rest, [flip runProperty testable])
runTest' (TestGroup name tests) results = (RunTestGroup name run_tests, results', requested_runs)
  where (run_tests, results', requested_runs) = runTests' tests results
runTest' _ _ = error "runTest': incoming results did not match outgoing ones"

runTests' :: [Test] -> [TestResult] -> ([RunTest], [TestResult], [PendingTest])
runTests' initial_tests initial_results = (reverse final_run_tests, final_results, final_requested_runs)
  where
    (final_run_tests, final_results, final_requested_runs) = foldl' go ([], initial_results, []) initial_tests
    
    go (run_tests, results, requested_runs) test = (run_test : run_tests, results', requested_runs ++ requested_runs')
      where (run_test, results', requested_runs') = runTest' test results