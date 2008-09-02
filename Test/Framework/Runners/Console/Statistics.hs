module Test.Framework.Runners.Console.Statistics (
        TestCount(..), tc_total,
        TestStatistics(..), ts_pending_tests, ts_any_failures, initialTestStatistics, showFinalTestStatistics
    ) where

import Test.Framework.Runners.Console.Table

import Data.Monoid


-- | Records a count of the various kinds of test that have been run
data TestCount = TestCount {
        tc_properties :: Int
    }

-- | The number of tests of all kinds recorded in the given 'TestCount'
tc_total :: TestCount -> Int
tc_total = tc_properties

instance Monoid TestCount where
    mempty = TestCount {
            tc_properties = 0
        }
    
    mappend tc1 tc2 = TestCount {
            tc_properties = tc_properties tc1 + tc_properties tc2
        }

minusTestCount :: TestCount -> TestCount -> TestCount
minusTestCount tc1 tc2 = TestCount {
        tc_properties = tc_properties tc1 - tc_properties tc2
    }


-- | Records information about the run of a number of tests, such
-- as how many tests have been run, how many are pending and how
-- many have passed or failed.
data TestStatistics = TestStatistics {
        ts_total_tests :: TestCount,
        ts_run_tests :: TestCount,
        ts_passed_tests :: TestCount,
        ts_failed_tests :: TestCount
    }

ts_pending_tests :: TestStatistics -> TestCount
ts_pending_tests ts = ts_total_tests ts `minusTestCount` ts_run_tests ts

ts_any_failures :: TestStatistics -> Bool
ts_any_failures ts = tc_total (ts_failed_tests ts) /= 0

-- | Create some test statistics that simply records the total number of
-- tests to be run, ready to be updated by the actual test runs.
initialTestStatistics :: TestCount -> TestStatistics
initialTestStatistics total_tests = TestStatistics {
        ts_total_tests = total_tests,
        ts_run_tests = mempty,
        ts_passed_tests = mempty,
        ts_failed_tests = mempty
    }

-- | Displays statistics as a string something like this:
--
-- @
--        | Properties | Total
-- -------+------------+------
-- Passed | 9          | 9
-- Failed | 1          | 1
-- -------+------------+------
-- Total  | 10         | 10
-- @
showFinalTestStatistics :: TestStatistics -> String
showFinalTestStatistics ts = renderTable [Column label_column, SeperatorColumn, Column properties_column, SeperatorColumn, Column total_column]
  where
    label_column      = [Text "",           Seperator, Text "Passed",                Text "Failed",                Seperator, Text "Total"]
    properties_column = [Text "Properties", Seperator, propertyStat ts_passed_tests, propertyStat ts_failed_tests, Seperator, propertyStat ts_total_tests]
    total_column      = [Text "Total",      Seperator, totalStat ts_passed_tests,    totalStat ts_failed_tests,    Seperator, totalStat ts_total_tests]
    
    propertyStat = stat tc_properties
    totalStat    = stat tc_total
    stat kind accessor = Text (show $ kind $ accessor ts)