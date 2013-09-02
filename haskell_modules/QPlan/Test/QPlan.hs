import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

main :: IO ()
main = defaultMainWithOpts
        [ testCase "rev" testRev] mempty

testRev :: Assertion
testRev = reverse [1, 2, 3] @?= [3, 2, 1]
