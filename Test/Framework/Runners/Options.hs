module Test.Framework.Runners.Options where

import Test.Framework.Options
import Test.Framework.Utilities

import Data.Monoid


type RunnerOptions = RunnerOptions' Maybe
type CompleteRunnerOptions = RunnerOptions' K
data RunnerOptions' f = RunnerOptions {
        ropt_threads :: f Int,
        ropt_test_options :: f (TestOptions' f)
    }

instance Monoid (RunnerOptions' Maybe) where
    mempty = RunnerOptions {
            ropt_threads = Nothing,
            ropt_test_options = Nothing
        }

    mappend ro1 ro2 = RunnerOptions {
            ropt_threads = getLast (mappendBy (Last . ropt_threads) ro1 ro2),
            ropt_test_options = mappendBy ropt_test_options ro1 ro2
        }