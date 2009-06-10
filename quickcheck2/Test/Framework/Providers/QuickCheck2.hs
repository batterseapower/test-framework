-- | Allows QuickCheck2 properties to be used with the test-framework package.
--
-- For an example of how to use test-framework, please see <http://github.com/batterseapower/test-framework/raw/master/example/Test/Framework/Example.lhs>
--
-- Due to technical limitations of the QuickCheck2 package, it's not possible to correlate arguments that caused
-- the failure with the particular 'Test' that caused them. This means that the arguments to your properties that
-- lead to failure will be printed at non-deterministic positions in the stdout stream. Sorry.
module Test.Framework.Providers.QuickCheck2 (
        testProperty
    ) where

import Test.Framework.Providers.API

import Test.QuickCheck.Gen
import Test.QuickCheck.Property hiding ( Property )
import Test.QuickCheck.Test ( run, maxSize, stdArgs, callbackPostTest, callbackPostFinalFailure )
import Test.QuickCheck.Text
import Test.QuickCheck.State

import qualified Control.Exception.Extensible as E

import Data.List

import System.Random


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

data PropertyStatus = PropertyOK                 -- ^ The property is true as far as we could check it
                    | PropertyArgumentsExhausted -- ^ The property may be true, but we ran out of arguments to try it out on
                    | PropertyFalsifiable String -- ^ The property was not true. The string is the reason.
                    | PropertyNoExpectedFailure  -- ^ We expected that a property would fail but it didn't
                    | PropertyTimedOut           -- ^ The property timed out during execution
                    | PropertyException String   -- ^ The property raised an exception during execution

instance Show PropertyResult where
    show (PropertyResult { pr_status = status, pr_used_seed = used_seed, pr_tests_run = mb_tests_run })
      = case status of
            PropertyOK                      -> "OK, passed " ++ tests_run_str ++ " tests"
            PropertyArgumentsExhausted      -> "Arguments exhausted after " ++ tests_run_str ++ " tests"
            PropertyFalsifiable fail_reason -> "Falsifiable with seed " ++ show used_seed ++ ", after " ++ tests_run_str ++ " tests. Reason: " ++ fail_reason
            PropertyNoExpectedFailure       -> "No expected failure with seed " ++ show used_seed ++ ", after " ++ tests_run_str ++ " tests"
            PropertyTimedOut                -> "Timed out after " ++ tests_run_str ++ " tests"
            PropertyException text          -> "Got an exception: " ++ text
      where
        tests_run_str = fmap show mb_tests_run `orElse` "an unknown number of"

propertySucceeded :: PropertyResult -> Bool
propertySucceeded property_result = propertyStatusIsSuccess (pr_status property_result)

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
    (seed, state) <- initialState topts
    runImprovingIO $ do
        mb_result <- maybeTimeoutImprovingIO (unK (topt_timeout topts)) $ myTest state (unGen (property testable))
        return $ toPropertyResult seed $ case mb_result of
            Nothing                  -> (PropertyTimedOut, Nothing)
            Just (status, tests_run) -> (status, Just tests_run)
  where
    toPropertyResult seed (status, mb_tests_run) = PropertyResult {
            pr_status = status,
            pr_used_seed = seed,
            pr_tests_run = mb_tests_run
        }


-- I've copied these parts out of QuickCheck 2 source code and modified them to fit my purpose. My
-- central problem with using the code as-is is that it insists on writing to stdout!

initialState :: CompleteTestOptions -> IO (Int, State)
initialState topts = do
    (gen, seed) <- newSeededStdGen (unK $ topt_seed topts)
    
    -- My code is very careful not to write to the terminal since it will screw up my own
    -- output code, but I need to fill in the terminal field anyway
    tm <- newTerminal
    
    let max_success = unK $ topt_maximum_generated_tests topts
        max_size = maxSize stdArgs -- Maximum generated value size currently not configurable
    return $ (seed, MkState {
          terminal          = tm
        , maxSuccessTests   = unK $ topt_maximum_generated_tests topts
        , maxDiscardedTests = unK $ topt_maximum_unsuitable_generated_tests topts
        , computeSize       = \n d -> (n * max_size) `div` max_success + (d `div` 10)
        , numSuccessTests   = 0
        , numDiscardedTests = 0
        , collected         = []
        , expectedFailure   = False
        , randomSeed        = gen
        , isShrinking       = False
        , numSuccessShrinks = 0
        , numTryShrinks     = 0 })


-- NB: could use (summary st) in the messages produced by myTest
myTest :: State -> (StdGen -> Int -> Prop) -> ImprovingIO PropertyTestCount f (PropertyStatus, PropertyTestCount)
myTest st f
  | ntest                >= maxSuccessTests st   = return (if expectedFailure st then PropertyOK else PropertyNoExpectedFailure, ntest)
  | numDiscardedTests st >= maxDiscardedTests st = return (PropertyArgumentsExhausted, ntest)
  | otherwise                                    = yieldImprovement ntest >> myRunATest st f
  where ntest = numSuccessTests st

myRunATest :: State -> (StdGen -> Int -> Prop) -> ImprovingIO PropertyTestCount f (PropertyStatus, PropertyTestCount)
myRunATest st f = do
    let size = computeSize st (numSuccessTests st) (numDiscardedTests st)
        (rnd1, rnd2) = split (randomSeed st)
    -- Careful to catch exceptions, or else they might bring down the whole test framework
    ei_st_res <- liftIO $ E.catch (fmap Right $ run (unProp (f rnd1 size)))
                                  (\e -> return $ Left $ show (e :: E.SomeException))
    case ei_st_res of
       Left text -> return (PropertyException text, numSuccessTests st + 1)
       Right (res, ts) -> do
           liftIO $ callbackPostTest st res
    
           case ok res of
              Just True -> -- successful test
                do myTest st{ numSuccessTests = numSuccessTests st + 1
                            , randomSeed      = rnd2
                            , collected       = stamp res : collected st
                            , expectedFailure = expect res
                            } f
       
              Nothing -> -- discarded test
                do myTest st{ numDiscardedTests = numDiscardedTests st + 1
                            , randomSeed        = rnd2
                            , expectedFailure   = expect res
                            } f
         
              Just False -> -- failed test
                do if expect res
                     then liftIO $ myFoundFailure st res ts
                          -- Could terminate immediately without any shrinking by doing this instead:
                          -- return (PropertyFalsifiable (reason res), numSuccessTests st + 1)
                     else return (PropertyOK, numSuccessTests st + 1)


-- | This function eventually reports a failure but attempts to shrink the counterexample before it does so
myFoundFailure :: State -> Result -> [Rose (IO Result)] -> IO (PropertyStatus, PropertyTestCount)
myFoundFailure st res ts = myLocalMin st{ numTryShrinks = 0, isShrinking = True } res ts

myLocalMin :: State -> Result -> [Rose (IO Result)] -> IO (PropertyStatus, PropertyTestCount)
myLocalMin st res [] = do
    callbackPostFinalFailure st res
    -- NB: could use (numSuccessShrinks st) in the message somehow
    return (PropertyFalsifiable (reason res), numSuccessTests st + 1)

myLocalMin st res (t : ts) =
  do (res', ts') <- run t
     callbackPostTest st res'
     
     -- NB: both (numSuccessShrinks st) (numTryShrinks st) contain interesting information here.
     -- I'm not going to output any message at all here (unlike QuickCheck2) because I want a
     -- single error message I can give to the user.
     if ok res' == Just False
       then myFoundFailure st{ numSuccessShrinks = numSuccessShrinks st + 1 } res' ts'
       else myLocalMin st{ numTryShrinks = numTryShrinks st + 1 } res ts