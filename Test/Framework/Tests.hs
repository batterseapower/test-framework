module Main where

import qualified Test.Framework.Tests.Runners.ThreadPool as TP

import Test.HUnit


-- I wish I could use my test framework to test my framework...
main :: IO ()
main = do
    runTestTT $ TestList TP.tests
    return ()