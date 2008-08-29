module Test.Framework.QuickCheck (
        PropertyResult, propertySucceeded,
        runProperty,
        
        -- Re-exports from QuickCheck
        Testable(..)
    ) where

import Test.Framework.Options
import Test.Framework.Seed
import Test.Framework.Utilities

import Test.QuickCheck

import Data.List

import System.Random


-- | The failure information from the run of a property
data PropertyResult = PropertyResult {
        pr_status :: PropertyStatus,
        pr_used_seed :: Int,
        pr_tests_run :: Int
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


runProperty :: Testable a => CompleteTestOptions -> a -> IO PropertyResult
runProperty topts testable = do
    (gen, seed) <- newSeededStdGen (unK $ topt_seed topts)
    (status, tests_run) <- myCheck (unK $ topt_quickcheck_options topts) gen testable
    return $ PropertyResult {
            pr_status = status,
            pr_used_seed = seed,
            pr_tests_run = tests_run
        }

-- The following somewhat ripped out of the QuickCheck source code so that
-- I can customise the random number generator used to do the checking etc
myCheck :: (Testable a) => CompleteQuickCheckOptions -> StdGen -> a -> IO (PropertyStatus, Int)
myCheck qcoptions rnd a = myTests qcoptions (evaluate a) rnd 0 0 []

myTests :: CompleteQuickCheckOptions -> Gen Result -> StdGen -> Int -> Int -> [[String]] -> IO (PropertyStatus, Int)
myTests qcoptions gen rnd0 ntest nfail stamps
  | ntest == unK (qcopt_maximum_tests qcoptions)    = do return (PropertyOK, ntest)
  | nfail == unK (qcopt_maximum_failures qcoptions) = do return (PropertyArgumentsExhausted, ntest)
  | otherwise               =
      do case ok result of
           Nothing    ->
             myTests qcoptions gen rnd1 ntest (nfail + 1) stamps
           Just True  ->
             myTests qcoptions gen rnd1 (ntest + 1) nfail (stamp result:stamps)
           Just False -> do
             return $ (PropertyFalsifiable (arguments result), ntest)
  where
    result       = generate (configSize defaultConfig ntest) rnd2 gen
    (rnd1, rnd2) = split rnd0