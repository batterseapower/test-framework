module Test.Framework.Providers.HUnit (
        testCase
    ) where

import Test.Framework.Core
import Test.Framework.Improving
import Test.Framework.Options
import Test.Framework.Utilities

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