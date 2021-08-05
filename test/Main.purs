module Test.Main where

import Prelude

import Effect (Effect)
import Test.Shannon.MigrationBuilder (migrationTests)
import Test.Shannon.Type.SerializeSchema (serializeSchemaTests)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  serializeSchemaTests
  migrationTests
