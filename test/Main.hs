
module Main where

import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Tests.Data.Docker.Image.V1        as ImageV1Tests
import qualified Tests.Data.Docker.Image.V1_2      as ImageV1_2Tests
import qualified Tests.Data.Docker.Nix.FetchDocker as FetchDockerTests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ ImageV1Tests.unitTests
  , ImageV1_2Tests.unitTests
  , FetchDockerTests.tests
  ]
