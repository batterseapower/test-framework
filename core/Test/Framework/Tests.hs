module Main where

import qualified Test.Framework.Tests.Runners.ThreadPool as TP
import qualified Test.Framework.Tests.Runners.XMLTests as XT

import Test.HUnit
import Test.QuickCheck

-- I wish I could use my test framework to test my framework...
main :: IO ()
main = do
    _ <- runTestTT $ TestList [
    	TestList TP.tests,
    	XT.test
      ]
    quickCheck XT.property
