module Test.Framework.Runners.Console (
        defaultMain, defaultMainWithArgs, defaultMainWithOpts
    ) where

import Test.Framework.Core
import Test.Framework.Improving
import Test.Framework.Options
import Test.Framework.Processors
import Test.Framework.QuickCheck
import Test.Framework.Runners.Console.ProgressBar
import Test.Framework.Runners.Console.Statistics
import Test.Framework.Runners.Console.Utilities
import Test.Framework.Runners.Core
import Test.Framework.Runners.Options
import Test.Framework.Seed
import Test.Framework.Utilities

import qualified Test.QuickCheck as QC

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
    run_tests <- runTests (unK $ ropt_threads ropts') (unK $ ropt_test_options ropts') tests
    
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


totalRunTests :: RunTest -> TestCount
totalRunTests (RunProperty _ _)      = mempty { tc_properties = 1 }
totalRunTests (RunTestGroup _ tests) = totalRunTestsList tests

totalRunTestsList :: [RunTest] -> TestCount
totalRunTestsList = mconcat . map totalRunTests


-- This code all /really/ sucks.  There must be a better way to seperate out the console-updating
-- and the improvement-traversing concerns - but how?
showRunTest :: Int -> TestStatistics -> RunTest -> IO TestStatistics
showRunTest indent_level test_statistics (RunProperty name improving_result) = do
    let run_tests    = tc_total (ts_run_tests test_statistics)
        total_tests  = tc_total (ts_total_tests test_statistics)
        any_failures = ts_any_failures test_statistics
        progress_bar = showProgressBar (if any_failures then red else green) 80 (Progress run_tests total_tests)
    property_suceeded <- showImprovingPropertyResult (return ()) indent_level name progress_bar improving_result
    let test_statistics' = test_statistics {
            ts_run_tests    = ts_run_tests test_statistics    `mappend` (mempty { tc_properties = 1 }),
            ts_failed_tests = ts_failed_tests test_statistics `mappend` (mempty { tc_properties = if property_suceeded then 0 else 1 }),
            ts_passed_tests = ts_passed_tests test_statistics `mappend` (mempty { tc_properties = if property_suceeded then 1 else 0 })
        }
    return test_statistics'
showRunTest indent_level test_statistics (RunTestGroup name tests) = do
    putDoc $ indent indent_level (text name <> char ':' <> linebreak)
    showRunTests (indent_level + 2) test_statistics tests

showRunTests :: Int -> TestStatistics -> [RunTest] -> IO TestStatistics
showRunTests indent_level = foldM (showRunTest indent_level)

showImprovingPropertyResult :: IO () -> Int -> String -> Doc -> (PropertyTestCount :~> PropertyResult) -> IO Bool
showImprovingPropertyResult erase indent_level test_name _ (Finished result) = do
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
    success = propertySucceeded result
    (result_doc, extra_doc) | success   = (brackets $ green (text (show result)), empty)
                            | otherwise = (brackets (red (text "Failed")), text (show result) <> linebreak)
showImprovingPropertyResult erase indent_level test_name progress_bar (Improving tests rest) = do
    erase
    putTestHeader indent_level test_name (brackets (text tests_str))
    when (tests `mod` 10 == 0) $ putDoc progress_bar
    hFlush stdout
    showImprovingPropertyResult (previousLine 1 >> clearLine) indent_level test_name progress_bar rest
  where  
    tests_str = show tests

putTestHeader :: Int -> String -> Doc -> IO ()
putTestHeader indent_level test_name result = putDoc $ indent indent_level (text test_name <> char ':' <+> result) <> linebreak