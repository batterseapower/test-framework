module Test.Framework.Providers.QuickCheck (
        testProperty
    ) where

import Test.Framework.Core
import Test.Framework.Improving
import Test.Framework.Options
import Test.Framework.Seed
import Test.Framework.Utilities

import Test.QuickCheck hiding (Property)

import Data.List

import System.Random


-- | Create a 'Test' for a QuickCheck 'Testable' property
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
        pr_tests_run :: PropertyTestCount
    }

data PropertyStatus = PropertyOK                   -- ^ The property is true as far as we could check it
                    | PropertyArgumentsExhausted   -- ^ The property may be true, but we ran out of arguments to try it out on
                    | PropertyFalsifiable [String] -- ^ The property was not true. The list of strings are the arguments inducing failure.

instance Show PropertyResult where
    show (PropertyResult { pr_status = status, pr_used_seed = used_seed, pr_tests_run = tests_run })
      = case status of
            PropertyOK                    -> "OK, passed " ++ show tests_run ++ " tests"
            PropertyArgumentsExhausted    -> "Arguments exhausted after " ++ show tests_run ++ " tests"
            PropertyFalsifiable test_args -> "Falsifiable with seed " ++ show used_seed ++ ", after " ++ show tests_run ++ " tests:\n" ++ unlinesConcise test_args

propertySucceeded :: PropertyResult -> Bool
propertySucceeded result = propertyStatusIsSuccess (pr_status result)

propertyStatusIsSuccess :: PropertyStatus -> Bool
propertyStatusIsSuccess PropertyOK                 = True
propertyStatusIsSuccess PropertyArgumentsExhausted = True
propertyStatusIsSuccess _                          = False


data Property = forall a. Testable a => Property a

instance Testlike PropertyTestCount PropertyResult Property where
    runTest topts (Property testable) = runProperty topts testable
    testTypeName _ = "Properties"

runProperty :: Testable a => CompleteTestOptions -> a -> IO (PropertyTestCount :~> PropertyResult, IO ())
runProperty topts testable = do
    (gen, seed) <- newSeededStdGen (unK $ topt_seed topts)
    runImprovingIO $ fmap (toPropertyResult seed) $ myCheck (unK $ topt_quickcheck_options topts) gen testable
  where
    toPropertyResult seed (status, tests_run) = PropertyResult {
            pr_status = status,
            pr_used_seed = seed,
            pr_tests_run = tests_run
        }

myCheck :: (Testable a) => CompleteQuickCheckOptions -> StdGen -> a -> ImprovingIO PropertyTestCount f (PropertyStatus, PropertyTestCount)
myCheck qcoptions rnd a = myTests qcoptions (evaluate a) rnd 0 0 []

myTests :: CompleteQuickCheckOptions -> Gen Result -> StdGen -> PropertyTestCount -> PropertyTestCount -> [[String]] -> ImprovingIO PropertyTestCount f (PropertyStatus, PropertyTestCount)
myTests qcoptions gen rnd0 ntest nfail stamps
  | ntest == unK (qcopt_maximum_tests qcoptions)    = do return (PropertyOK, ntest)
  | nfail == unK (qcopt_maximum_failures qcoptions) = do return (PropertyArgumentsExhausted, ntest)
  | otherwise = do
      yieldImprovement ntest
      case ok result of
          Nothing    ->
            myTests qcoptions gen rnd1 ntest (nfail + 1) stamps
          Just True  ->
            myTests qcoptions gen rnd1 (ntest + 1) nfail (stamp result:stamps)
          Just False ->
            return (PropertyFalsifiable (arguments result), ntest)
  where
    result       = generate (configSize defaultConfig ntest) rnd2 gen
    (rnd1, rnd2) = split rnd0