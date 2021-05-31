module StackTests (test_Stack) where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Test.Tasty.SmallCheck as SC

import Stack

test_Stack :: TestTree
test_Stack = testGroup "Tests" [unitTests, props]

unitTests = testGroup "ListStack"
  [
    testCase "empty List" $
      isEmpty (ListStack [1,2])  @?= False

  , testCase "Creates new List" $
      (new :: ListStack Int) @?= ListStack []

  , testCase "Consing / Push" $
      cons 3 (ListStack [1]) @?= ListStack [3, 1]

  , testCase "Pop" $
      pop (ListStack [3, 1]) @?= (Just 3, ListStack [1])

  ]

props = testGroup "ListStack properties"
  [
    SC.testProperty "cons == push" $
      \list x ->  push (x :: Int) (ListStack (list :: [Int])) == push (x :: Int) (ListStack (list :: [Int]))
  ]
