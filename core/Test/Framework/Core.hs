module Test.Framework.Core where

import Test.Framework.Improving
import Test.Framework.Options


-- | Something like the result of a test: works in concert with 'Testlike'.
-- The type parameters are the type that is used for progress reports and the
-- type of the final output of the test respectively.
class (Show i, Show r) => TestResultlike i r | r -> i where
    testSucceeded :: r -> Bool

-- | Something test-like in its behaviour. The type parameters are the type that
-- is used for progress reports, the type of the final output of the test and the
-- data type encapsulating the whole potential to do a test respectively.
class TestResultlike i r => Testlike i r t | t -> i r, r -> i where
    runTest :: CompleteTestOptions -> t -> IO (i :~> r, IO ())
    testTypeName :: t -> TestTypeName


-- | Test names or descriptions. These are shown to the user
type TestName = String

-- | The name of a type of test, such as "Properties" or "Test Cases". Tests of
-- types of the same names will be grouped together in the test run summary.
type TestTypeName = String

-- | Main test data type: builds up a list of tests to be run. Users should use the
-- utility functions in e.g. the test-framework-hunit and test-framework-quickcheck
-- packages to create instances of 'Test', and then build them up into testsuites
-- by using 'testGroup' and lists.
--
-- For an example of how to use test-framework, please see
-- <http://github.com/batterseapower/test-framework/raw/master/example/Test/Framework/Example.lhs>
data Test = forall i r t. Testlike i r t => Test TestName t -- ^ A single test of some particular type. 
          | TestGroup TestName [Test]
          | PlusTestOptions TestOptions Test

-- | Assemble a number of tests into a cohesive group
testGroup :: TestName -> [Test] -> Test
testGroup = TestGroup

plusTestOptions :: TestOptions -> Test -> Test
plusTestOptions = PlusTestOptions