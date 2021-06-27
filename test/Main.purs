module Test.Main where

import Prelude

import Effect (Effect)
import Test.Unit (test)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  test "can have test" mempty
