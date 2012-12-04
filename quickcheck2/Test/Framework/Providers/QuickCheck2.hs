{-# LANGUAGE DeriveDataTypeable #-}
-- | Allows QuickCheck2 properties to be used with the test-framework package.
--
-- For an example of how to use test-framework, please see <http://github.com/batterseapower/test-framework/raw/master/example/Test/Framework/Example.lhs>
module Test.Framework.Providers.QuickCheck2 (
        testProperty
    ) where

import Test.Framework.Providers.API

import Test.QuickCheck.Gen
import Test.QuickCheck.Property hiding ( Property, Result( reason, interrupted ) )
import qualified Test.QuickCheck.Property as P
import Test.QuickCheck.Test
import Test.QuickCheck.Text
import Test.QuickCheck.State

import Control.Concurrent.MVar
import qualified Control.Exception.Extensible as E

import Data.IORef
import Data.Typeable
import System.Random
import Unsafe.Coerce


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
    (seed, mk_state) <- initialState topts
    runImprovingIO $ do
        mb_result <- maybeTimeoutImprovingIO (unK (topt_timeout topts)) $ do
          (state, get_out) <- liftIO mk_state
          myTest state get_out (unGen (property testable))
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

initialState :: CompleteTestOptions -> IO (Int, IO (State, IO String))
initialState topts = do
    (gen, seed) <- newSeededStdGen (unK $ topt_seed topts)
    
    let max_success = unK $ topt_maximum_generated_tests topts
        max_size = unK $ topt_maximum_test_size topts

        -- Copied from the unexported function Test.QuickCheck.Text.output
        -- Very horrible hack here since the Output data constructor is also not exported!
        output f = do
          r <- newIORef ""
          return (unsafeCoerce (TestQuickCheckTextOutput f r))

        mk_state = do
          -- My code is very careful not to write to the terminal since it will screw up my own
          -- output code, but I need to fill in the terminal field anyway. This is useful for
          -- catching the failing examples that get written to the output.
          out_var <- newMVar ""
          out <- output $ \extra -> modifyMVar_ out_var $ \str -> return (str ++ extra)
          tm <- newTerminal out out

          return (MkState { terminal          = tm
                          , maxSuccessTests   = unK $ topt_maximum_generated_tests topts
                          , maxDiscardedTests = unK $ topt_maximum_unsuitable_generated_tests topts
                          , computeSize       = \n d -> (n * max_size) `div` max_success + (d `div` 10)
                          , numSuccessTests   = 0
                          , numDiscardedTests = 0
                          , collected         = []
                          , expectedFailure   = False
                          , randomSeed        = gen
                          , numSuccessShrinks = 0
                          , numTryShrinks     = 0
#if MIN_VERSION_QuickCheck(2,5,0)
                          , numTotTryShrinks  = 0
#endif
                          },
                  modifyMVar out_var $ \str -> return ("", str))
    return (seed, mk_state)

-- If this doesn't exactly match the definition of Test.QuickCheck.Text.Out you can get segfaults
data TestQuickCheckTextOutput = TestQuickCheckTextOutput (String -> IO ()) (IORef String)


-- NB: could use (summary st) in the messages produced by myTest
myTest :: State -> IO String -> (StdGen -> Int -> Prop) -> ImprovingIO PropertyTestCount f (PropertyStatus, PropertyTestCount)
myTest st get_out f
  | ntest                >= maxSuccessTests st   = return (if expectedFailure st then PropertyOK else PropertyNoExpectedFailure, ntest)
  | numDiscardedTests st >= maxDiscardedTests st = return (PropertyArgumentsExhausted, ntest)
  | otherwise                                    = yieldImprovement ntest >> myRunATest st get_out f
  where ntest = numSuccessTests st

myRunATest :: State -> IO String -> (StdGen -> Int -> Prop) -> ImprovingIO PropertyTestCount f (PropertyStatus, PropertyTestCount)
myRunATest st get_out f = do
    let size = computeSize st (numSuccessTests st) (numDiscardedTests st)
    -- Careful to catch exceptions, or else they might bring down the whole test framework
    ei_st_res <- liftIO $ flip E.catch (\e -> return $ Left $ show (e :: E.SomeException)) $ do
                                  MkRose res ts <- protectRose (reduceRose (unProp (f rnd1 size)))
                                  return (Right (res, ts))
                                  
    case ei_st_res of
       Left text -> liftIO get_out >>= \extra_text -> return (PropertyException (text ++ extra_text), numSuccessTests st + 1)
       Right (res, ts) -> do
           liftIO $ callbackPostTest st res
    
           case res of
              MkResult{ok = Just True, stamp = stamp, expect = expect} -> -- successful test
                do myTest st{ numSuccessTests = numSuccessTests st + 1
                            , randomSeed      = rnd2
                            , collected       = stamp : collected st
                            , expectedFailure = expect
                            } get_out f
       
              MkResult{ok = Nothing, expect = expect} -> -- discarded test
                do myTest st{ numDiscardedTests = numDiscardedTests st + 1
                            , randomSeed        = rnd2
                            , expectedFailure   = expect
                            } get_out f
         
              MkResult{ok = Just False} -> -- failed test
                do if expect res
                     then liftIO $ myFoundFailure st get_out res ts
                          -- Could terminate immediately without any shrinking by doing this instead:
                          -- return (PropertyFalsifiable (reason res), numSuccessTests st + 1)
                     else return (PropertyOK, numSuccessTests st + 1)
  where
   (rnd1,rnd2) = split (randomSeed st)


-- | This function eventually reports a failure but attempts to shrink the counterexample before it does so
myFoundFailure :: State -> IO String -> P.Result -> [Rose P.Result] -> IO (PropertyStatus, PropertyTestCount)
myFoundFailure st get_out res ts =
  do myLocalMin st{ numTryShrinks = 0 } get_out res ts

myLocalMin :: State -> IO String -> P.Result -> [Rose P.Result] -> IO (PropertyStatus, PropertyTestCount)
myLocalMin st get_out res _ | P.interrupted res = myLocalMinFound st get_out res
myLocalMin st get_out res ts = do
    r <- tryEvaluate ts
    case r of
      Left err ->
        myLocalMinFound st get_out
           (exception "Exception while generating shrink-list" err) { callbacks = callbacks res }
      Right ts' -> myLocalMin' st get_out res ts'


myLocalMin' :: State -> IO String -> P.Result -> [Rose P.Result] -> IO (PropertyStatus, PropertyTestCount)
myLocalMin' st get_out res [] = myLocalMinFound st get_out res
myLocalMin' st get_out res (t:ts) =
  do -- CALLBACK before_test
    MkRose res' ts' <- protectRose (reduceRose t)
    callbackPostTest st res'
    -- NB: both (numSuccessShrinks st) (numTryShrinks st) contain interesting information here.
    -- I'm not going to output any message at all here (unlike QuickCheck2) because I want a
    -- single error message I can give to the user.
    if ok res' == Just False
      then myFoundFailure st{ numSuccessShrinks = numSuccessShrinks st + 1 } get_out res' ts'
      else myLocalMin st{ numTryShrinks = numTryShrinks st + 1 } get_out res ts

myLocalMinFound :: State -> IO String -> P.Result -> IO (PropertyStatus, PropertyTestCount)
myLocalMinFound st get_out res =
  do callbackPostFinalFailure st res
     -- NB: could use (numSuccessShrinks st) in the message somehow
     extra_text <- get_out
     let reason = dropWhileRev (`elem` "\r\n") extra_text ++
                  if P.reason res /= "Falsifiable" then P.reason res else ""
     return (PropertyFalsifiable reason, numSuccessTests st + 1)

dropWhileRev :: (a -> Bool) -> [a] -> [a]
dropWhileRev p = reverse . dropWhile p . reverse


--
-- Hidden module Test.QuickCheck.Exception
--

tryEvaluate :: a -> IO (Either E.SomeException a)
tryEvaluate x = tryEvaluateIO (return x)

tryEvaluateIO :: IO a -> IO (Either E.SomeException a)
tryEvaluateIO m = E.try (m >>= E.evaluate)
--tryEvaluateIO m = Right `fmap` m
