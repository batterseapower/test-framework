module Test.Framework.Runners.Core (
        RunTest(..), runTests
    ) where

import Test.Framework.Core
import Test.Framework.Improving
import Test.Framework.Options
import Test.Framework.Runners.Options
import Test.Framework.Runners.TestPattern
import Test.Framework.Runners.ThreadPool
import Test.Framework.Seed
import Test.Framework.Utilities

import Control.Monad

import Data.List
import Data.Maybe
import Data.Monoid


-- | A test that has been executed
data RunTest = forall i r. TestResultlike i r => RunTest TestName TestTypeName (i :~> r)
             | RunTestGroup TestName [RunTest]

runTests :: CompleteRunnerOptions -- ^ Top-level runner options
         -> [Test]                -- ^ Tests to run
         -> IO [RunTest]
runTests ropts tests = do
    let test_patterns = unK $ ropt_test_patterns ropts
        tests' | null test_patterns = tests
               | otherwise          = filterTests test_patterns [] tests
    (run_tests, actions) <- runTests' (unK $ ropt_test_options ropts) tests'
    executeOnPool (unK $ ropt_threads ropts) actions
    return run_tests


filterTests :: [TestPattern] -> [String] -> [Test] -> [Test]
filterTests patterns path = mapMaybe (filterTest patterns path)

filterTest :: [TestPattern] -> [String] -> Test -> Maybe Test
filterTest patterns path test@(Test name _)
  | any (`testPatternMatches` (path ++ [name])) patterns = Just test
  | otherwise                                            = Nothing
filterTest patterns path (TestGroup name tests)
  | null tests' = Nothing
  | otherwise   = Just (TestGroup name tests')
  where
    tests' = filterTests patterns (path ++ [name]) tests
filterTest patterns path (PlusTestOptions topts inner_test)
  = fmap (PlusTestOptions topts) (filterTest patterns path inner_test)


runTest' :: TestOptions -> Test -> IO (RunTest, [IO ()])
runTest' topts (Test name testlike) = do
    (result, action) <- runTest (completeTestOptions topts) testlike
    return (RunTest name (testTypeName testlike) result, [action])
runTest' topts (TestGroup name tests) = fmap (onLeft (RunTestGroup name)) $ runTests' topts tests
runTest' topts (PlusTestOptions extra_topts test) = runTest' (topts `mappend` extra_topts) test

runTests' :: TestOptions -> [Test] -> IO ([RunTest], [IO ()])
runTests' topts = fmap (onRight concat . unzip) . mapM (runTest' topts)


completeTestOptions :: TestOptions -> CompleteTestOptions
completeTestOptions to = TestOptions {
            topt_seed = K $ topt_seed to `orElse` RandomSeed,
            topt_maximum_generated_tests = K $ topt_maximum_generated_tests to `orElse` 100,
            topt_maximum_unsuitable_generated_tests = K $ topt_maximum_unsuitable_generated_tests to `orElse` 1000,
            topt_timeout = K $ topt_timeout to `orElse` Nothing
        }