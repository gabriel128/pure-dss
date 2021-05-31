import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import StackTests

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
  test_Stack
  ]
