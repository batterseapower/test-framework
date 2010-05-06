module Test.Framework.Runners.Common (
        defaultMain, defaultMainWithArgs, defaultMainWithOpts
    ) where

import Test.Framework.Core
import Test.Framework.Options
import qualified Test.Framework.Runners.Console as Console
import Test.Framework.Runners.Core
import Test.Framework.Runners.Options
import Test.Framework.Runners.Processors
import Test.Framework.Runners.Statistics
import Test.Framework.Seed
import Test.Framework.Utilities

import System.Console.GetOpt
import System.Environment
import System.Exit

import Data.Monoid


instance Functor OptDescr where
    fmap f (Option a b arg_descr c) = Option a b (fmap f arg_descr) c

instance Functor ArgDescr where
    fmap f (NoArg a) = NoArg (f a)
    fmap f (ReqArg g s) = ReqArg (f . g) s
    fmap f (OptArg g s) = OptArg (f . g) s

-- | @Nothing@ signifies that usage information should be displayed.
-- @Just@ simply gives us the contribution to overall options by the command line option.
type SuppliedRunnerOptions = Maybe RunnerOptions

optionsDescription :: [OptDescr SuppliedRunnerOptions]
optionsDescription = [
        Option [] ["help"]
            (NoArg Nothing)
            "show this help message"
    ] ++ map (fmap Just) [
        Option ['j'] ["threads"]
            (ReqArg (\t -> mempty { ropt_threads = Just (read t) }) "NUMBER")
            "number of threads to use to run tests",
        Option [] ["test-seed"]
            (ReqArg (\t -> mempty { ropt_test_options = Just (mempty { topt_seed = Just (read t) }) }) ("NUMBER|" ++ show RandomSeed))
            "default seed for test random number generator",
        Option ['a'] ["maximum-generated-tests"]
            (ReqArg (\t -> mempty { ropt_test_options = Just (mempty { topt_maximum_generated_tests = Just (read t) }) }) "NUMBER")
            "how many automated tests something like QuickCheck should try, by default",
        Option [] ["maximum-unsuitable-generated-tests"]
            (ReqArg (\t -> mempty { ropt_test_options = Just (mempty { topt_maximum_unsuitable_generated_tests = Just (read t) }) }) "NUMBER")
            "how many unsuitable candidate tests something like QuickCheck should endure before giving up, by default",
        Option ['o'] ["timeout"]
            (ReqArg (\t -> mempty { ropt_test_options = Just (mempty { topt_timeout = Just (Just (secondsToMicroseconds (read t))) }) }) "NUMBER")
            "how many seconds a test should be run for before giving up, by default",
        Option [] ["no-timeout"]
            (NoArg (mempty { ropt_test_options = Just (mempty { topt_timeout = Just Nothing }) }))
            "specifies that tests should be run without a timeout, by default",
        Option ['t'] ["select-tests"]
            (ReqArg (\t -> mempty { ropt_test_patterns = Just [read t] }) "TEST-PATTERN")
            "only tests that match at least one glob pattern given by an instance of this argument will be run"
    ]

interpretArgs :: [String] -> IO (Either String (RunnerOptions, [String]))
interpretArgs args = do
    prog_name <- getProgName
    let usage_header = "Usage: " ++ prog_name ++ " [OPTIONS]"
    
    case getOpt Permute optionsDescription args of
        (oas, n, []) | Just os <- sequence oas -> return $ Right (mconcat os, n)
        (_, _, errs)                           -> return $ Left (concat errs ++ usageInfo usage_header optionsDescription)


defaultMain :: [Test] -> IO ()
defaultMain tests = do
    args <- getArgs
    defaultMainWithArgs tests args

defaultMainWithArgs :: [Test] -> [String] -> IO ()
defaultMainWithArgs tests args = do
    interpreted_args <- interpretArgs args
    case interpreted_args of
        Right (ropts, [])    -> defaultMainWithOpts tests ropts
        Right (_, leftovers) -> do
            putStrLn $ "Could not understand these extra arguments: " ++ unwords leftovers
            exitWith (ExitFailure 1)
        Left error_message   -> do
            putStrLn error_message
            exitWith (ExitFailure 1)

defaultMainWithOpts :: [Test] -> RunnerOptions -> IO ()
defaultMainWithOpts tests ropts = do
    let ropts' = completeRunnerOptions ropts
    
    -- Get a lazy list of the test results, as executed in parallel
    running_tests <- runTests ropts' tests
    
    -- Show those test results to the user as we get them
    run_tests <- Console.showRunTestsTop running_tests
    
    -- Set the error code depending on whether the tests succeded or not
    let test_statistics' = gatherStatistics run_tests
    exitWith $ if ts_no_failures test_statistics'
               then ExitSuccess
               else ExitFailure 1


completeRunnerOptions :: RunnerOptions -> CompleteRunnerOptions
completeRunnerOptions ro = RunnerOptions {
            ropt_threads = K $ ropt_threads ro `orElse` processorCount,
            ropt_test_options = K $ ropt_test_options ro `orElse` mempty,
            ropt_test_patterns = K $ ropt_test_patterns ro `orElse` mempty
        }
