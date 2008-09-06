module Test.Framework.Runners.Console (
        defaultMain, defaultMainWithArgs, defaultMainWithOpts
    ) where

import Test.Framework.Core
import Test.Framework.Improving
import Test.Framework.Options
import Test.Framework.Runners.Console.ProgressBar
import Test.Framework.Runners.Console.Statistics
import Test.Framework.Runners.Console.Utilities
import Test.Framework.Runners.Core
import Test.Framework.Runners.Options
import Test.Framework.Runners.Processors
import Test.Framework.Runners.TimedConsumption
import Test.Framework.Seed
import Test.Framework.Utilities

import System.Console.ANSI
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

import Text.PrettyPrint.ANSI.Leijen

import Data.List
import Data.Maybe
import Data.Monoid

import Control.Monad


optionsDescription :: [OptDescr RunnerOptions]
optionsDescription = [
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
            (ReqArg (\t -> mempty { ropt_test_patterns = Just [read t] }) "[!]((TEST_OR_CATEGORY_NAME | * | **) [/])+")
            "only tests that match at least one glob pattern given by an instance of this argument will be run"
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
        Right (_, leftovers) -> do
            putStrLn $ "Could not understand these extra arguments: " ++ unwords leftovers
            exitWith (ExitFailure 1)
        Left error_message   -> do
            putStrLn error_message
            exitWith (ExitFailure 1)

defaultMainWithOpts :: [Test] -> RunnerOptions -> IO ()
defaultMainWithOpts tests ropts = hideCursorIn $ do
    let ropts' = completeRunnerOptions ropts
    
    -- Get a lazy list of the test results, as executed in parallel
    run_tests <- runTests ropts' tests
    
    -- Show those test results to the user as we get them
    let test_statistics = initialTestStatistics (totalRunTestsList run_tests)
    test_statistics' <- showRunTests 0 test_statistics run_tests
    
    -- Show the final statistics
    putStrLn ""
    putStr $ showFinalTestStatistics test_statistics'
    
    -- Set the error code depending on whether the tests succeded or not
    exitWith $ if ts_any_failures test_statistics'
               then ExitFailure 1
               else ExitSuccess


completeRunnerOptions :: RunnerOptions -> CompleteRunnerOptions
completeRunnerOptions ro = RunnerOptions {
            ropt_threads = K $ ropt_threads ro `orElse` processorCount,
            ropt_test_options = K $ ropt_test_options ro `orElse` mempty,
            ropt_test_patterns = K $ ropt_test_patterns ro `orElse` mempty
        }


totalRunTests :: RunTest -> TestCount
totalRunTests (RunTest _ test_type _) = adjustTestCount test_type 1 mempty
totalRunTests (RunTestGroup _ tests)  = totalRunTestsList tests

totalRunTestsList :: [RunTest] -> TestCount
totalRunTestsList = mconcat . map totalRunTests


-- This code all /really/ sucks.  There must be a better way to seperate out the console-updating
-- and the improvement-traversing concerns - but how?
showRunTest :: Int -> TestStatistics -> RunTest -> IO TestStatistics
showRunTest indent_level test_statistics (RunTest name test_type improving_result) = do
    let progress_bar = testStatisticsProgressBar test_statistics
    property_suceeded <- showImprovingTestResult (return ()) indent_level name progress_bar improving_result
    return $ updateTestStatistics (\count -> adjustTestCount test_type count mempty) property_suceeded test_statistics
showRunTest indent_level test_statistics (RunTestGroup name tests) = do
    putDoc $ indent indent_level (text name <> char ':' <> linebreak)
    showRunTests (indent_level + 2) test_statistics tests

showRunTests :: Int -> TestStatistics -> [RunTest] -> IO TestStatistics
showRunTests indent_level = foldM (showRunTest indent_level)


testStatisticsProgressBar :: TestStatistics -> Doc
testStatisticsProgressBar test_statistics = showProgressBar (if any_failures then red else green) 80 (Progress run_tests total_tests)
  where
    run_tests    = testCountTotal (ts_run_tests test_statistics)
    total_tests  = testCountTotal (ts_total_tests test_statistics)
    any_failures = ts_any_failures test_statistics

updateTestStatistics :: (Int -> TestCount) -> Bool -> TestStatistics -> TestStatistics
updateTestStatistics count_constructor test_suceeded test_statistics = test_statistics {
        ts_run_tests    = ts_run_tests test_statistics    `mappend` (count_constructor 1),
        ts_failed_tests = ts_failed_tests test_statistics `mappend` (count_constructor (if test_suceeded then 0 else 1)),
        ts_passed_tests = ts_passed_tests test_statistics `mappend` (count_constructor (if test_suceeded then 1 else 0))
    }


consumeImprovingThing :: (a :~> b) -> [(a :~> b)]
consumeImprovingThing improving@(Finished _)       = [improving]
consumeImprovingThing improving@(Improving _ rest) = improving : consumeImprovingThing rest


showImprovingTestResult :: TestResultlike i r => IO () -> Int -> String -> Doc -> (i :~> r) -> IO Bool
showImprovingTestResult erase indent_level test_name progress_bar improving = do
    -- Update the screen every every 200ms
    improving_list <- consumeListInInterval 200000 (consumeImprovingThing improving)
    case listToMaybeLast improving_list of
        Nothing         -> do -- 200ms was somehow not long enough for a single result to arrive: try again!
            showImprovingTestResult erase indent_level test_name progress_bar improving
        Just improving' -> do -- Display that new improving value to the user
            showImprovingTestResult' erase indent_level test_name progress_bar improving'

showImprovingTestResult' :: TestResultlike i r => IO () -> Int -> String -> Doc -> (i :~> r) -> IO Bool
showImprovingTestResult' erase indent_level test_name _ (Finished result) = do
    erase
    -- Output the final test status and a trailing newline
    putTestHeader indent_level test_name result_doc
    -- There may still be a progress bar on the line below the final test result, so 
    -- remove it as a precautionary measure in case this is the last test in a group
    -- and hence it will not be erased in the normal course of test display.
    clearLine
    -- Output any extra information that may be required, e.g. to show failure reason
    putDoc extra_doc
    return success
  where
    success = testSucceeded result
    (result_doc, extra_doc) | success   = (brackets $ green (text (show result)), empty)
                            | otherwise = (brackets (red (text "Failed")), text (show result) <> linebreak)
showImprovingTestResult' erase indent_level test_name progress_bar (Improving intermediate rest) = do
    erase
    putTestHeader indent_level test_name (brackets (text intermediate_str))
    putDoc progress_bar
    hFlush stdout
    showImprovingTestResult (previousLine 1 >> clearLine) indent_level test_name progress_bar rest
  where  
    intermediate_str = show intermediate

putTestHeader :: Int -> String -> Doc -> IO ()
putTestHeader indent_level test_name result = putDoc $ indent indent_level (text test_name <> char ':' <+> result) <> linebreak