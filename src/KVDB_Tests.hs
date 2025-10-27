module KVDB_Tests (tests) where

import KVDB
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Control.Concurrent

tests :: TestTree
tests =
  testGroup
    "KVDB"
    [testCase "put 1, get 1" $ do
      c <- startKVDB :: IO (KVDB Integer Integer)
      kvPut c 1 0
      a <- kvGet c 1
      a @?= 0,
      testCase "put (1,0), put(0,1), get 1" $ do
      c <- startKVDB :: IO (KVDB Integer Integer)
      kvPut c 1 0
      kvPut c 0 1
      a <- kvGet c 1
      a @?= 0,
      testCase "put (1,0), put (1, 10) get 1" $ do
      c <- startKVDB :: IO (KVDB Integer Integer)
      kvPut c 1 0
      kvPut c 1 10
      a <- kvGet c 1
      a @?= 10,
      testCase "put (k,0), put (a, 10) get k" $ do
      c <- startKVDB :: IO (KVDB String Integer)
      kvPut c "k" 0
      kvPut c "a" 10
      a <- kvGet c "a"
      a @?= 10,
      testCase "Thread with delay puts 0,10, kvget 0" $ do
      c <- startKVDB :: IO (KVDB Integer Integer)
      _ <- forkIO $ do
        threadDelay 1000000
        kvPut c 0 10
      a <- kvGet c 0
      a @?= 10
    ]
