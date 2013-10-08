{-# LANGUAGE DeriveDataTypeable #-}
-- | Allows QuickCheck2 properties to be used with the test-framework package.
--
-- For an example of how to use test-framework, please see <http://github.com/batterseapower/test-framework/raw/master/example/Test/Framework/Example.lhs>
module Test.Framework.Providers.QuickCheck2 (
        testProperty
    ) where

import Test.Framework.Providers.API

import Test.QuickCheck.Property (Testable, Callback(PostTest), CallbackKind(NotCounterexample), callback)
import Test.QuickCheck.State (numSuccessTests)
import Test.QuickCheck.Test

import Data.Typeable


-- | Create a 'Test' for a QuickCheck2 'Testable' property
testProperty :: Testable a => TestName -> a -> Test
testProperty name = Test name . Property


instance TestResultlike PropertyTestCount PropertyResult where
    testSucceeded = propertySucceeded

-- | Used to document numbers which we expect to be intermediate test counts from running properties
type PropertyTestCount = Int

-- | The failure information from the run of a property
data PropertyResult = PropertyResult {
        pr_status :: PropertyStatus,
        pr_used_seed :: Int,
        pr_tests_run :: Maybe PropertyTestCount -- Due to technical limitations, it's currently not possible to find out the number of
                                                -- tests previously run if the test times out, hence we need a Maybe here for that case.
    }

data PropertyStatus = PropertyOK                        -- ^ The property is true as far as we could check it
                    | PropertyArgumentsExhausted        -- ^ The property may be true, but we ran out of arguments to try it out on
                    | PropertyFalsifiable String String -- ^ The property was not true. The strings are the reason and the output.
                    | PropertyNoExpectedFailure         -- ^ We expected that a property would fail but it didn't
                    | PropertyTimedOut                  -- ^ The property timed out during execution

instance Show PropertyResult where
    show (PropertyResult { pr_status = status, pr_used_seed = used_seed, pr_tests_run = mb_tests_run })
      = case status of
            PropertyOK                    -> "OK, passed " ++ tests_run_str ++ " tests"
            PropertyArgumentsExhausted    -> "Arguments exhausted after " ++ tests_run_str ++ " tests"
            PropertyFalsifiable _rsn otpt -> otpt ++ "(used seed " ++ show used_seed ++ ")"
            PropertyNoExpectedFailure     -> "No expected failure with seed " ++ show used_seed ++ ", after " ++ tests_run_str ++ " tests"
            PropertyTimedOut              -> "Timed out after " ++ tests_run_str ++ " tests"
      where
        tests_run_str = fmap show mb_tests_run `orElse` "an unknown number of"

propertySucceeded :: PropertyResult -> Bool
propertySucceeded (PropertyResult { pr_status = status, pr_tests_run = mb_n }) = case status of
  PropertyOK                 -> True
  PropertyArgumentsExhausted -> maybe False (/= 0) mb_n
  _                          -> False


data Property = forall a. Testable a => Property a
    deriving Typeable

instance Testlike PropertyTestCount PropertyResult Property where
    runTest topts (Property testable) = runProperty topts testable
    testTypeName _ = "Properties"

runProperty :: Testable a => CompleteTestOptions -> a -> IO (PropertyTestCount :~> PropertyResult, IO ())
runProperty topts testable = do
    (gen, seed) <- newSeededStdGen (unK $ topt_seed topts)
    let max_success = unK $ topt_maximum_generated_tests topts
        max_discard = unK $ topt_maximum_unsuitable_generated_tests topts
        args = stdArgs { replay = Just (gen, 0) -- NB: the 0 is the saved size. Defaults to 0 if you supply "Nothing" for "replay".
                       , maxSuccess = max_success
#if MIN_VERSION_QuickCheck(2,5,0)
                       , maxDiscardRatio = (max_discard `div` max_success) + 1
#else
                       , maxDiscard = max_discard
#endif
                       , maxSize = unK $ topt_maximum_test_size topts
                       , chatty = False }
    -- FIXME: yield gradual improvement after each test
    runImprovingIO $ do
        tunnel <- tunnelImprovingIO
        mb_result <- maybeTimeoutImprovingIO (unK (topt_timeout topts)) $
          liftIO $ quickCheckWithResult args (callback (PostTest NotCounterexample (\s _r -> tunnel $ yieldImprovement $ numSuccessTests s)) testable)
        return $ case mb_result of
            Nothing     -> PropertyResult { pr_status = PropertyTimedOut, pr_used_seed = seed, pr_tests_run = Nothing }
            Just result -> PropertyResult {
                   pr_status = toPropertyStatus result,
                   pr_used_seed = seed,
                   pr_tests_run = Just (numTests result)
               }
  where
    toPropertyStatus (Success {})                              = PropertyOK
    toPropertyStatus (GaveUp {})                               = PropertyArgumentsExhausted
    toPropertyStatus (Failure { reason = rsn, output = otpt }) = PropertyFalsifiable rsn otpt
    toPropertyStatus (NoExpectedFailure {})                    = PropertyNoExpectedFailure
