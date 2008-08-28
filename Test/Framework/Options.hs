module Test.Framework.Options where

import Test.Framework.Seed
import Test.Framework.Utilities

import qualified Test.QuickCheck as QC

import Data.Monoid


type TestOptions = TestOptions' Maybe
type CompleteTestOptions = TestOptions' K
data TestOptions' f = TestOptions {
        topt_seed :: f Seed,
        topt_quickcheck_options :: f (QuickCheckOptions' f)
    }

instance Monoid (TestOptions' Maybe) where
    mempty = TestOptions {
            topt_seed = Nothing,
            topt_quickcheck_options = Nothing
        }
    
    mappend to1 to2 = TestOptions {
            topt_seed = getLast (mappendBy (Last . topt_seed) to1 to2),
            topt_quickcheck_options = mappendBy topt_quickcheck_options to1 to2
        }

type QuickCheckOptions = QuickCheckOptions' Maybe
type CompleteQuickCheckOptions = QuickCheckOptions' K
data QuickCheckOptions' f = QuickCheckOptions {
        qcopt_maximum_tests :: f Int,
        qcopt_maximum_failures :: f Int
    }

instance Monoid (QuickCheckOptions' Maybe) where
    mempty = QuickCheckOptions {
            qcopt_maximum_tests = Nothing,
            qcopt_maximum_failures = Nothing
        }
    
    mappend to1 to2 = QuickCheckOptions {
            qcopt_maximum_tests = getLast (mappendBy (Last . qcopt_maximum_tests) to1 to2),
            qcopt_maximum_failures = getLast (mappendBy (Last . qcopt_maximum_failures) to1 to2)
        }