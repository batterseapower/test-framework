module Test.Framework.Runners.Core (
        RunTest(..), runTests
    ) where

import Test.Framework.Core
import Test.Framework.Improving
import Test.Framework.Options
import Test.Framework.QuickCheck
import Test.Framework.ThreadPool
import Test.Framework.Utilities

import Control.Monad

import Data.List


-- | A test that has been executed
data RunTest = RunProperty TestName (TestCount :~> PropertyResult)
             | RunTestGroup TestName [RunTest]

runTests :: Int                  -- ^ Number of threads to use to execute the tests
          -> CompleteTestOptions -- ^ Top-level test options
          -> [Test]              -- ^ Tests to run
          -> IO [RunTest]
runTests threads topts tests = do
    (run_tests, actions) <- runTests' topts tests
    executeOnPool threads actions
    return run_tests

runTest' :: CompleteTestOptions -> Test -> IO (RunTest, [IO ()])
runTest' topts (Property name testable) = do
    (result, action) <- runProperty topts testable
    return (RunProperty name result, [action])
runTest' topts (TestGroup name tests) = fmap (onLeft (RunTestGroup name)) $ runTests' topts tests

runTests' :: CompleteTestOptions -> [Test] -> IO ([RunTest], [IO ()])
runTests' topts = fmap (onRight concat . unzip) . mapM (runTest' topts)
