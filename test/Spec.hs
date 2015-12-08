module Spec where

import Network.Wai.Session.VCache.SimpleSpec

import Test.Tasty


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Testing..."
  [spec]
