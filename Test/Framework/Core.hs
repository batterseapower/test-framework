module Test.Framework.Core where

import Test.QuickCheck


-- | Test names or descriptions. These are shown to the user
type TestName = String

-- | Main test data type: build up a list of tests to be run with this.
data Test = forall a. Testable a => Property TestName a
          | TestGroup TestName [Test]

-- | Used to document numbers which we expect to be test counts
type TestCount = Int
