
module Main where

import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Tests.Data.Docker.Image           as Docker.Image
import qualified Tests.Data.Docker.Nix.FetchDocker as FetchDockerTests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ Docker.Image.unitTests
  , FetchDockerTests.tests
  ]
