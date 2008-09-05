module Main where

import qualified Test.Framework.Tests.Runners.ThreadPool as TP

import Test.HUnit
--import Test.QuickCheck


-- I wish I could use my test framework to test my framework...
main :: IO ()
main = do
    -- HUnit tests first
    runTestTT $ TestList TP.tests
    -- QuickCheck tests, if we add any
    return ()