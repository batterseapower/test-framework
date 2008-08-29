module Test.Framework.Runners.Console (
        defaultMain, defaultMainWithArgs, defaultMainWithOpts
    ) where

import Test.Framework.Core
import Test.Framework.Improving
import Test.Framework.Options
import Test.Framework.Processors
import Test.Framework.QuickCheck
import Test.Framework.Runners.Core
import Test.Framework.Runners.Options
import Test.Framework.Seed
import Test.Framework.Utilities

import qualified Test.QuickCheck as QC

import System.Console.GetOpt
import System.Environment
import System.Exit

import Data.Maybe
import Data.Monoid


optionsDescription :: [OptDescr RunnerOptions]
optionsDescription = [
        Option ['j'] ["threads"]
            (ReqArg (\t -> mempty { ropt_threads = Just (read t) }) "NUMBER")
            "number of threads to use to run tests",
        Option [] ["test-seed"]
            (ReqArg (\t -> mempty { ropt_test_options = Just (mempty { topt_seed = Just (read t) }) }) ("NUMBER|" ++ show RandomSeed))
            "default seed for test random number generator",
        Option ['t'] ["qc-maximum-tests"]
            (ReqArg (\t -> mempty { ropt_test_options = Just (mempty { topt_quickcheck_options = Just (mempty { qcopt_maximum_tests = Just (read t) }) }) }) "NUMBER")
            "how many tests QuickCheck should try, by default",
        Option [] ["qc-maximum-failures"]
            (ReqArg (\t -> mempty { ropt_test_options = Just (mempty { topt_quickcheck_options = Just (mempty { qcopt_maximum_failures = Just (read t) }) }) }) "NUMBER")
            "how many unsuitable candidate bits of test data QuickCheck will endure before giving up, by default"
    ]

interpretArgs :: [String] -> IO (Either String (RunnerOptions, [String]))
interpretArgs args = do
    prog_name <- getProgName
    let usage_header = "Usage: " ++ prog_name ++ " [OPTIONS]"
    
    case getOpt Permute optionsDescription args of
        (o, n, [])   -> return $ Right (mconcat o, n)
        (_, _, errs) -> return $ Left (concat errs ++ usageInfo usage_header optionsDescription)


defaultMain :: [Test] -> IO ()
defaultMain tests = do
    args <- getArgs
    defaultMainWithArgs tests args

defaultMainWithArgs :: [Test] -> [String] -> IO ()
defaultMainWithArgs tests args = do
    interpreted_args <- interpretArgs args
    case interpreted_args of
        Right (ropts, [])    -> defaultMainWithOpts tests ropts
        Right (_, leftovers) -> putStrLn $ "Could not understand these extra arguments: " ++ unwords leftovers
        Left error_message   -> putStrLn error_message

defaultMainWithOpts :: [Test] -> RunnerOptions -> IO ()
defaultMainWithOpts tests ropts = do
    let ropts' = completeRunnerOptions ropts
    
    -- Get a lazy list of the test results, as executed in parallel
    run_tests <- runTests (unK $ ropt_threads ropts') (unK $ ropt_test_options ropts') tests
    
    -- Show those test results to the user as we get them
    result <- showRunTests 0 run_tests
    exitWith $ if result
               then ExitSuccess
               else ExitFailure 1


completeRunnerOptions :: RunnerOptions -> CompleteRunnerOptions
completeRunnerOptions ro = RunnerOptions {
            ropt_threads = K $ ropt_threads ro `orElse` processorCount,
            ropt_test_options = K $ completeTestOptions (ropt_test_options ro `orElse` mempty)
        }

completeTestOptions :: TestOptions -> CompleteTestOptions
completeTestOptions to = TestOptions {
            topt_seed = K $ topt_seed to `orElse` RandomSeed,
            topt_quickcheck_options = K $ completeQuickCheckOptions (topt_quickcheck_options to `orElse` mempty)
        }

completeQuickCheckOptions :: QuickCheckOptions -> CompleteQuickCheckOptions
completeQuickCheckOptions qco = QuickCheckOptions {
            qcopt_maximum_tests = K $ qcopt_maximum_tests qco `orElse` QC.configMaxTest QC.defaultConfig,
            qcopt_maximum_failures = K $ qcopt_maximum_failures qco `orElse` QC.configMaxFail QC.defaultConfig
        }


showRunTest :: Int -> RunTest -> IO Bool
showRunTest indent_level (RunProperty name improving_result) = do
    putStrIndented indent_level (name ++ ": ")
    result <- showImprovingPropertyResult 0 improving_result
    return (propertySucceeded result)
showRunTest indent_level (RunTestGroup name tests) = do
    putStrLnIndented indent_level (name ++ ":")
    showRunTests (indent_level + 2) tests

showRunTests :: Int -> [RunTest] -> IO Bool
showRunTests indent_level = fmap and . mapM (showRunTest indent_level)

showImprovingPropertyResult :: Int -> (TestCount :~> PropertyResult) -> IO PropertyResult
showImprovingPropertyResult erase_amount (Finished result) = do
    eraseStr erase_amount
    putStrLn (show result)
    return result
showImprovingPropertyResult erase_amount (Improving tests rest) = do
    eraseStr erase_amount
    putStr tests_str
    showImprovingPropertyResult (length tests_str) rest
  where  
    tests_str = show tests
