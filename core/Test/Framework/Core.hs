module Test.Framework.Core where

import Test.Framework.Improving
import Test.Framework.Options


-- | Something like the result of a test: works in concert with 'Testlike'
class (Show i, Show r) => TestResultlike i r | r -> i where
    testSucceeded :: r -> Bool

-- | Something test-like in its behaviour
class TestResultlike i r => Testlike i r t | t -> i r, r -> i where
    runTest :: CompleteTestOptions -> t -> IO (i :~> r, IO ())
    testTypeName :: t -> TestTypeName


-- | Test names or descriptions. These are shown to the user
type TestName = String

-- | The name of a type of test, such as "Properties" or "Test Cases"
type TestTypeName = String

-- | Main test data type: build up a list of tests to be run with this.
data Test = forall i r t. Testlike i r t => Test TestName t
          | TestGroup TestName [Test]
          | PlusTestOptions TestOptions Test

testGroup :: TestName -> [Test] -> Test
testGroup = TestGroup

plusTestOptions :: TestOptions -> Test -> Test
plusTestOptions = PlusTestOptions