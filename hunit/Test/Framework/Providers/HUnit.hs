-- | Allows HUnit test cases to be used with the test-framework package.
--
-- For an example of how to use test-framework, please see <http://github.com/batterseapower/test-framework/raw/master/example/Test/Framework/Example.lhs>
module Test.Framework.Providers.HUnit (
        testCase,
        hUnitTestToTests,
    ) where

import Test.Framework.Providers.API

import qualified Test.HUnit.Base
import Test.HUnit.Lang


-- | Create a 'Test' for a HUnit 'Assertion'
testCase :: TestName -> Assertion -> Test
testCase name = Test name . TestCase

-- | Adapt an existing HUnit test into a list of test-framework tests.
-- This is useful when migrating your existing HUnit test suite to test-framework.
hUnitTestToTests :: Test.HUnit.Base.Test -> [Test]
hUnitTestToTests = go ""
  where
    go desc (Test.HUnit.Base.TestCase a)    = [testCase desc a]
    go desc (Test.HUnit.Base.TestLabel s t) = go (desc ++ ":" ++ s) t
    go desc (Test.HUnit.Base.TestList ts)
        -- If the list occurs at the top level (with no description above it),
        -- just return that list straightforwardly
      | null desc = concatMap (go desc) ts
        -- If the list occurs with a description, turn that into a honest-to-god
        -- test group. This is heuristic, but likely to give good results
      | otherwise = [testGroup desc (concatMap (go "") ts)]


instance TestResultlike TestCaseRunning TestCaseResult where
    testSucceeded = testCaseSucceeded

data TestCaseRunning = TestCaseRunning

instance Show TestCaseRunning where
    show TestCaseRunning = "Running"

data TestCaseResult = TestCasePassed
                    | TestCaseFailed String
                    | TestCaseError String

instance Show TestCaseResult where
    show result = case result of
        TestCasePassed         -> "OK"
        TestCaseFailed message -> message
        TestCaseError message  -> "ERROR: " ++ message

testCaseSucceeded :: TestCaseResult -> Bool
testCaseSucceeded TestCasePassed = True
testCaseSucceeded _              = False


newtype TestCase = TestCase Assertion

instance Testlike TestCaseRunning TestCaseResult TestCase where
    runTest topts (TestCase assertion) = runTestCase topts assertion
    testTypeName _ = "Test Cases"

runTestCase :: CompleteTestOptions -> Assertion -> IO (TestCaseRunning :~> TestCaseResult, IO ())
runTestCase topts assertion = runImprovingIO $ do
    yieldImprovement TestCaseRunning
    mb_result <- maybeTimeoutImprovingIO (unK $ topt_timeout topts) $ liftIO (myPerformTestCase assertion)
    return (mb_result `orElse` TestCaseError "Timed out")

myPerformTestCase :: Assertion -> IO TestCaseResult
myPerformTestCase assertion = do
    result <- performTestCase assertion
    return $ case result of
        Nothing               -> TestCasePassed
        Just (True, message)  -> TestCaseFailed message
        Just (False, message) -> TestCaseError message