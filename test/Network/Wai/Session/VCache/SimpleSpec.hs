module Network.Wai.Session.VCache.SimpleSpec (spec) where

import Network.Wai.Session.VCache.Simple

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck
import Test.QuickCheck.Instances


spec :: TestTree
spec = testGroup "Network.Wai.Session.VCache.Simple"
  [ QC.testProperty "`someFunction` should pass"
      someFunction
  ]

someFunction :: Bool -> Property
someFunction x = not (not $ x) === x
