module Test.Framework.QuickCheck (
        PropertyResult(..), propertySucceeded,
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
data PropertyResult = PropertyOK Int                   -- ^ The property is true as far as we could check it, passing the given number of tests.
                    | PropertyArgumentsExhausted Int   -- ^ The property may be true, but we ran out of arguments to try it out on.
                                                       -- We were only able to try the given number of tests.
                    | PropertyFalsifiable Int [String] -- ^ The property was not true. The @Int@ is the number of tests required to
                                                       -- discover this, and the list of strings are the arguments inducing failure.

instance Show PropertyResult where
    show (PropertyOK ntest)                     = "OK, passed " ++ show ntest ++ " tests"
    show (PropertyArgumentsExhausted ntest)     = "Arguments exhausted after " ++ show ntest ++ " tests"
    show (PropertyFalsifiable ntests test_args) = "Falsifiable, after " ++ show ntests ++ " tests:\n" ++ unlines test_args

propertySucceeded :: PropertyResult -> Bool
propertySucceeded (PropertyOK _)                 = True
propertySucceeded (PropertyArgumentsExhausted _) = True
propertySucceeded _                              = False


runProperty :: Testable a => CompleteTestOptions -> a -> IO PropertyResult
runProperty topts testable = do
    gen <- newSeededStdGen (unK $ topt_seed topts)
    myCheck (unK $ topt_quickcheck_config topts) gen testable

-- The following somewhat ripped out of the QuickCheck source code so that
-- I can customise the random number generator used to do the checking etc
myCheck :: (Testable a) => Config -> StdGen -> a -> IO PropertyResult
myCheck config rnd a = myTests config (evaluate a) rnd 0 0 []

myTests :: Config -> Gen Result -> StdGen -> Int -> Int -> [[String]] -> IO PropertyResult
myTests config gen rnd0 ntest nfail stamps
  | ntest == configMaxTest config = do return (PropertyOK ntest)
  | nfail == configMaxFail config = do return (PropertyArgumentsExhausted ntest)
  | otherwise               =
      do case ok result of
           Nothing    ->
             myTests config gen rnd1 ntest (nfail + 1) stamps
           Just True  ->
             myTests config gen rnd1 (ntest + 1) nfail (stamp result:stamps)
           Just False -> do
             return $ PropertyFalsifiable ntest (arguments result)
  where
    result      = generate (configSize config ntest) rnd2 gen
    (rnd1, rnd2) = split rnd0