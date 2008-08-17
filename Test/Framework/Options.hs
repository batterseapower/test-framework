module Test.Framework.Options where

import Test.Framework.Seed
import Test.Framework.Utilities

import qualified Test.QuickCheck as QC

import Data.Monoid


type TestOptions = TestOptions' Maybe
type CompleteTestOptions = TestOptions' K
data TestOptions' f = TestOptions {
        topt_seed :: f Seed,
        topt_quickcheck_config :: f QC.Config
    }

instance Monoid (TestOptions' Maybe) where
    mempty = TestOptions {
            topt_seed = Nothing,
            topt_quickcheck_config = Nothing
        }
    
    mappend to1 to2 = TestOptions {
            topt_seed = getLast (mappendBy (Last . topt_seed) to1 to2),
            topt_quickcheck_config = getLast (mappendBy (Last . topt_quickcheck_config) to1 to2)
        }