module Test.Framework.Runners.Console (
        defaultMain, defaultMainWithArgs, defaultMainWithOpts,
        interpretArgs, interpretArgsOrExit
    ) where

import Test.Framework.Core
import Test.Framework.Options
import Test.Framework.Runners.Console.Run
import Test.Framework.Runners.Core
import Test.Framework.Runners.Options
import Test.Framework.Runners.Processors
import Test.Framework.Runners.Statistics
import qualified Test.Framework.Runners.XML as XML
import Test.Framework.Seed
import Test.Framework.Utilities

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

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
        Option ['d'] ["maximum-test-depth"]
            (ReqArg (\t -> mempty { ropt_test_options = Just (mempty { topt_maximum_test_depth = Just (read t) }) }) "NUMBER")
            "to what depth something like SmallCheck should test the properties, by default",
        Option ['o'] ["timeout"]
            (ReqArg (\t -> mempty { ropt_test_options = Just (mempty { topt_timeout = Just (Just (secondsToMicroseconds (read t))) }) }) "NUMBER")
            "how many seconds a test should be run for before giving up, by default",
        Option [] ["no-timeout"]
            (NoArg (mempty { ropt_test_options = Just (mempty { topt_timeout = Just Nothing }) }))
            "specifies that tests should be run without a timeout, by default",
        Option ['t'] ["select-tests"]
            (ReqArg (\t -> mempty { ropt_test_patterns = Just [read t] }) "TEST-PATTERN")
            "only tests that match at least one glob pattern given by an instance of this argument will be run",
        Option [] ["jxml"]
            (ReqArg (\t -> mempty { ropt_xml_output = Just (Just t) }) "FILE")
            "write a JUnit XML summary of the output to FILE",
        Option [] ["jxml-nested"]
            (NoArg (mempty { ropt_xml_nested = Just True }))
            "use nested testsuites to represent groups in JUnit XML (not standards compliant)",
        Option [] ["plain"]
            (NoArg (mempty { ropt_plain_output = Just True }))
            "do not use any ANSI terminal features to display the test run",
        Option [] ["hide-successes"]
            (NoArg (mempty { ropt_hide_successes = Just True }))
            "hide sucessful tests, and only show failures"
    ]

-- | Parse the specified command line arguments into a 'RunnerOptions' and some remaining arguments,
-- or return a reason as to why we can't.
interpretArgs :: [String] -> IO (Either String (RunnerOptions, [String]))
interpretArgs args = do
    prog_name <- getProgName
    let usage_header = "Usage: " ++ prog_name ++ " [OPTIONS]"
    
    case getOpt Permute optionsDescription args of
        (oas, n, []) | Just os <- sequence oas -> return $ Right (mconcat os, n)
        (_, _, errs)                           -> return $ Left (concat errs ++ usageInfo usage_header optionsDescription)

-- | A version of 'interpretArgs' that ends the process if it fails.
interpretArgsOrExit :: [String] -> IO RunnerOptions
interpretArgsOrExit args = do
    interpreted_args <- interpretArgs args
    case interpreted_args of
        Right (ropts, [])    -> return ropts
        Right (_, leftovers) -> do
            hPutStrLn stderr $ "Could not understand these extra arguments: " ++ unwords leftovers
            exitWith (ExitFailure 1)
        Left error_message   -> do
            hPutStrLn stderr error_message
            exitWith (ExitFailure 1)


defaultMain :: [Test] -> IO ()
defaultMain tests = do
    args <- getArgs
    defaultMainWithArgs tests args

-- | A version of 'defaultMain' that lets you ignore the command line arguments
-- in favour of another list of 'String's.
defaultMainWithArgs :: [Test] -> [String] -> IO ()
defaultMainWithArgs tests args = do
    ropts <- interpretArgsOrExit args
    defaultMainWithOpts tests ropts

-- | A version of 'defaultMain' that lets you ignore the command line arguments
-- in favour of an explicit set of 'RunnerOptions'.
defaultMainWithOpts :: [Test] -> RunnerOptions -> IO ()
defaultMainWithOpts tests ropts = do
    let ropts' = completeRunnerOptions ropts
    
    -- Get a lazy list of the test results, as executed in parallel
    running_tests <- runTests ropts' tests
    
    -- Show those test results to the user as we get them
    fin_tests <- showRunTestsTop (unK $ ropt_plain_output ropts') (unK $ ropt_hide_successes ropts') running_tests
    let test_statistics' = gatherStatistics fin_tests
    
    -- Output XML report (if requested)
    case ropt_xml_output ropts' of
        K (Just file) -> XML.produceReport (unK (ropt_xml_nested ropts')) test_statistics' fin_tests >>= writeFile file
        _ -> return ()
    
    -- Set the error code depending on whether the tests succeded or not
    exitWith $ if ts_no_failures test_statistics'
               then ExitSuccess
               else ExitFailure 1


completeRunnerOptions :: RunnerOptions -> CompleteRunnerOptions
completeRunnerOptions ro = RunnerOptions {
            ropt_threads = K $ ropt_threads ro `orElse` processorCount,
            ropt_test_options = K $ ropt_test_options ro `orElse` mempty,
            ropt_test_patterns = K $ ropt_test_patterns ro `orElse` mempty,
            ropt_xml_output = K $ ropt_xml_output ro `orElse` Nothing,
            ropt_xml_nested = K $ ropt_xml_nested ro `orElse` False,
            ropt_plain_output = K $ ropt_plain_output ro `orElse` False,
            ropt_hide_successes = K $ ropt_hide_successes ro `orElse` False
        }
