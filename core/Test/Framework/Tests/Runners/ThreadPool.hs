module Test.Framework.Tests.Runners.ThreadPool (tests) where

import Test.Framework.Runners.ThreadPool

import Test.HUnit

import System.Random

import Control.Concurrent

import Prelude hiding (catch)


tests :: [Test]
tests = [TestLabel "ThreadPool.executeOnPool preserves order"                         (TestCase test_execute_preserves_order),
         TestLabel "ThreadPool.executeOnPool preserves order even with delays"        (TestCase test_execute_preserves_order_even_with_delay),
         TestLabel "ThreadPool.executeOnPool input list can depend on previous items" (TestCase test_execute_schedules_lazily)]

test_execute_preserves_order :: Assertion
test_execute_preserves_order = do
    let input = [1..1000] :: [Int]
    output <- executeOnPool 4 $ map return input
    input @=? output

test_execute_preserves_order_even_with_delay :: Assertion
test_execute_preserves_order_even_with_delay = do
    gen <- getStdGen
    let -- Execute 100 actions with a random delay of up to 50ms each
        input = [1..100] :: [Int]
        actions = zipWith (\n delay -> threadDelay delay >> return n) input (randomRs (0, 50000) gen)
    output <- executeOnPool 4 actions
    input @=? output

test_execute_schedules_lazily :: Assertion
test_execute_schedules_lazily = mdo
    ~(first_output:rest) <- executeOnPool 4 $ return 10 : (return 20) : replicate first_output (return 99) :: IO [Int]
    [10, 20] ++ (replicate 10 99) @=? (first_output:rest)