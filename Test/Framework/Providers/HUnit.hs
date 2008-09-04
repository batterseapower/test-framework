module Test.Framework.Providers.HUnit (
        testCase
    ) where

import Test.Framework.Core
import Test.Framework.Improving

import Test.HUnit.Lang


-- | Create a 'Test' for a HUnit 'Assertion'
testCase :: TestName -> Assertion -> Test
testCase name = Test name . TestCase


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
        TestCaseFailed message -> "Failed: " ++ message
        TestCaseError message  -> "ERROR: " ++ message

testCaseSucceeded :: TestCaseResult -> Bool
testCaseSucceeded TestCasePassed = True
testCaseSucceeded _              = False


newtype TestCase = TestCase Assertion

instance Testlike TestCaseRunning TestCaseResult TestCase where
    runTest _topts (TestCase assertion) = runTestCase assertion
    testTypeName _ = "Test Cases"

runTestCase :: Assertion -> IO (TestCaseRunning :~> TestCaseResult, IO ())
runTestCase assertion = runImprovingIO $ fmap toTestCaseResult $ yieldImprovement TestCaseRunning >> liftIO (performTestCase assertion)
  where
    toTestCaseResult Nothing                 = TestCasePassed
    toTestCaseResult (Just (True, message))  = TestCaseFailed message
    toTestCaseResult (Just (False, message)) = TestCaseError message