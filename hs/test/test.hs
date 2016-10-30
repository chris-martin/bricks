module Main (main) where

import qualified Test.DocTest

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

main :: IO ()
main = Test.DocTest.doctest ["src"]
