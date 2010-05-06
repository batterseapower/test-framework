module Test.Framework.Runners.Console (
        showRunTestsTop
    ) where

import Test.Framework.Core
import Test.Framework.Improving
import Test.Framework.Runners.Console.Colors
import Test.Framework.Runners.Console.ProgressBar
import Test.Framework.Runners.Console.Statistics
import Test.Framework.Runners.Console.Utilities
import Test.Framework.Runners.Core
import Test.Framework.Runners.Statistics
import Test.Framework.Runners.TimedConsumption
import Test.Framework.Utilities

import System.Console.ANSI
import System.IO

import Text.PrettyPrint.ANSI.Leijen

import Data.Monoid

import Control.Monad


showRunTestsTop :: TestStatistics -> [RunTest] -> IO TestStatistics
showRunTestsTop test_statistics run_tests = hideCursorDuring $ do
    -- Show those test results to the user as we get them
    test_statistics' <- showRunTests 0 test_statistics run_tests
    
    -- Show the final statistics
    putStrLn ""
    putDoc $ showFinalTestStatistics test_statistics'
    
    return test_statistics'


-- This code all /really/ sucks.  There must be a better way to seperate out the console-updating
-- and the improvement-traversing concerns - but how?
showRunTest :: Int -> TestStatistics -> RunTest -> IO TestStatistics
showRunTest indent_level test_statistics (RunTest name test_type improving_result) = do
    let progress_bar = testStatisticsProgressBar test_statistics
    property_suceeded <- showImprovingTestResult (return ()) indent_level name progress_bar improving_result
    return $ updateTestStatistics (\count -> adjustTestCount test_type count mempty) property_suceeded test_statistics
showRunTest indent_level test_statistics (RunTestGroup name tests) = do
    putDoc $ (indent indent_level (text name <> char ':')) <> linebreak
    showRunTests (indent_level + 2) test_statistics tests

showRunTests :: Int -> TestStatistics -> [RunTest] -> IO TestStatistics
showRunTests indent_level = foldM (showRunTest indent_level)


testStatisticsProgressBar :: TestStatistics -> Doc
testStatisticsProgressBar test_statistics = progressBar (colorPassOrFail no_failures) terminal_width (Progress run_tests total_tests)
  where
    run_tests   = testCountTotal (ts_run_tests test_statistics)
    total_tests = testCountTotal (ts_total_tests test_statistics)
    no_failures = ts_no_failures test_statistics
    -- We assume a terminal width of 80, but we can't make the progress bar 80 characters wide.  Why?  Because if we
    -- do so, when we write the progress bar out Windows will move the cursor onto the next line!  By using a slightly
    -- smaller width we prevent this from happening.  Bit of a hack, but it does the job.
    terminal_width = 79


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
    (result_doc, extra_doc) | success   = (brackets $ colorPass (text (show result)), empty)
                            | otherwise = (brackets (colorFail (text "Failed")), text (show result) <> linebreak)
showImprovingTestResult' erase indent_level test_name progress_bar (Improving intermediate rest) = do
    erase
    putTestHeader indent_level test_name (brackets (text intermediate_str))
    putDoc progress_bar
    hFlush stdout
    showImprovingTestResult (cursorUpLine 1 >> clearLine) indent_level test_name progress_bar rest
  where  
    intermediate_str = show intermediate

putTestHeader :: Int -> String -> Doc -> IO ()
putTestHeader indent_level test_name result = putDoc $ (indent indent_level (text test_name <> char ':' <+> result)) <> linebreak
