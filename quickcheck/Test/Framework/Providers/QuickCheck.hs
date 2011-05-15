-- | Allows QuickCheck1 properties to be used with the test-framework package.
--
-- For an example of how to use test-framework, please see <http://github.com/batterseapower/test-framework/raw/master/example/Test/Framework/Example.lhs>
module Test.Framework.Providers.QuickCheck (
        testProperty
    ) where

import Test.Framework.Providers.API

import Test.QuickCheck hiding (Property)

import qualified Control.Exception.Extensible as E
import Control.DeepSeq (rnf)

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
        pr_tests_run :: Maybe PropertyTestCount -- Due to technical limitations, it's currently not possible to find out the number of
                                                -- tests previously run if the test times out, hence we need a Maybe here for that case.
    }

data PropertyStatus = PropertyOK                   -- ^ The property is true as far as we could check it
                    | PropertyArgumentsExhausted   -- ^ The property may be true, but we ran out of arguments to try it out on
                    | PropertyFalsifiable [String] -- ^ The property was not true. The list of strings are the arguments inducing failure.
                    | PropertyTimedOut             -- ^ The property timed out during execution
                    | PropertyException String     -- ^ The property raised an exception during execution

instance Show PropertyResult where
    show (PropertyResult { pr_status = status, pr_used_seed = used_seed, pr_tests_run = mb_tests_run })
      = case status of
            PropertyOK                    -> "OK, passed " ++ tests_run_str ++ " tests"
            PropertyArgumentsExhausted    -> "Arguments exhausted after " ++ tests_run_str ++ " tests"
            PropertyFalsifiable test_args -> "Falsifiable with seed " ++ show used_seed ++ ", after " ++ tests_run_str ++ " tests:\n" ++ unlinesConcise test_args
            PropertyTimedOut              -> "Timed out after " ++ tests_run_str ++ " tests"
            PropertyException text        -> "Got an exception: " ++ text
      where
        tests_run_str = fmap show mb_tests_run `orElse` "an unknown number of"

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
    runImprovingIO $ do
        mb_result <- maybeTimeoutImprovingIO (unK (topt_timeout topts)) $ myCheck topts gen testable
        return $ toPropertyResult seed $ case mb_result of
            Nothing                  -> (PropertyTimedOut, Nothing)
            Just (status, tests_run) -> (status, Just tests_run)
  where
    toPropertyResult seed (status, mb_tests_run) = PropertyResult {
            pr_status = status,
            pr_used_seed = seed,
            pr_tests_run = mb_tests_run
        }

myCheck :: (Testable a) => CompleteTestOptions -> StdGen -> a -> ImprovingIO PropertyTestCount f (PropertyStatus, PropertyTestCount)
myCheck topts rnd a = myTests topts (evaluate a) rnd 0 0 []

myTests :: CompleteTestOptions -> Gen Result -> StdGen -> PropertyTestCount -> PropertyTestCount -> [[String]] -> ImprovingIO PropertyTestCount f (PropertyStatus, PropertyTestCount)
myTests topts gen rnd0 ntest nfail stamps
  | ntest == unK (topt_maximum_generated_tests topts)            = do return (PropertyOK, ntest)
  | nfail == unK (topt_maximum_unsuitable_generated_tests topts) = do return (PropertyArgumentsExhausted, ntest)
  | otherwise = do
      yieldImprovement ntest
      -- Rather clunky code that tries to catch exceptions early.  If we don't do this then errors
      -- in your properties just kill the test framework - very bad!
      mb_exception <- liftIO $ E.catch (fmap (const Nothing) $ E.evaluate (rnfResult result))
                                       (return . Just . show :: E.SomeException -> IO (Maybe String))
      case mb_exception of
          Just e -> return (PropertyException e, ntest)
          Nothing -> case ok result of
                        Nothing    ->
                          myTests topts gen rnd1 ntest (nfail + 1) stamps
                        Just True  ->
                          myTests topts gen rnd1 (ntest + 1) nfail (stamp result:stamps)
                        Just False ->
                          return (PropertyFalsifiable (arguments result), ntest)
  where
    result       = generate (configSize defaultConfig ntest) rnd2 gen
    (rnd1, rnd2) = split rnd0
    
    -- Reduce a Result to RNF before we poke at it in order to uncover hidden exceptions
    rnfResult r = rnf (ok r) `seq` rnf (stamp r) `seq` rnf (arguments r)