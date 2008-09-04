module Test.Framework.Options where

import Test.Framework.Seed
import Test.Framework.Utilities

import Data.Monoid


type TestOptions = TestOptions' Maybe
type CompleteTestOptions = TestOptions' K
data TestOptions' f = TestOptions {
        topt_seed :: f Seed,
        topt_maximum_generated_tests :: f Int,
        topt_maximum_unsuitable_generated_tests :: f Int
    }

instance Monoid (TestOptions' Maybe) where
    mempty = TestOptions {
            topt_seed = Nothing,
            topt_maximum_generated_tests = Nothing,
            topt_maximum_unsuitable_generated_tests = Nothing
        }
    
    mappend to1 to2 = TestOptions {
            topt_seed = getLast (mappendBy (Last . topt_seed) to1 to2),
            topt_maximum_generated_tests = getLast (mappendBy (Last . topt_maximum_generated_tests) to1 to2),
            topt_maximum_unsuitable_generated_tests = getLast (mappendBy (Last . topt_maximum_unsuitable_generated_tests) to1 to2)
        }