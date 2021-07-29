module Test.Shannon.Migration  where

import Prelude

import Data.Array as Array
import Data.NonEmpty as NonEmpty
import Foreign.Object as Object
import Shannon.Data.Migration (Migration(..))
import Shannon.Data.Proxy (inbound, incrementing, index, nonIncrementing, notUnique, outbound)
import Shannon.Migration (addIndex, addTable, defineMigration, newVersion)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Type.Data.Peano (d1, d5)
import Type.Proxy (Proxy(..))

_foo_ = Proxy :: Proxy "foo"
_bar_ = Proxy :: Proxy "bar"

_id_ = Proxy :: Proxy "id"
_age_ = Proxy :: Proxy "age"

migrationTests :: TestSuite
migrationTests = suite "migration" do
  let
    getVersions steps = map (_.version) steps # Array.reverse
    getStores steps = map (_.stores) steps # Array.reverse

  test "can make an empty migration" do
    let
      (Migration m) = defineMigration "mydb"

      steps = NonEmpty.oneOf m.steps

    Assert.shouldEqual m.dbName "mydb"
    Assert.shouldEqual (getVersions steps) [0]
    Assert.shouldEqual (getStores steps) [Object.empty]

  test "can make a migration with some version changes" do
    let
      (Migration m) = defineMigration "mydb"
        # newVersion d1
        # newVersion d5

      steps = NonEmpty.oneOf m.steps

    Assert.shouldEqual (getVersions steps) [0, 1, 5]
    Assert.shouldEqual (getStores steps) [Object.empty, Object.empty, Object.empty]

  test "can make a migration with some tables and indexes" do
    let
      (Migration m) = defineMigration "mydb"
        # addTable _foo_ (outbound incrementing)
        # addTable _bar_ (inbound nonIncrementing (index _id_))
        # addIndex _foo_ notUnique (index _age_)

      steps = NonEmpty.oneOf m.steps

    Assert.shouldEqual (getVersions steps) [0]
    Assert.shouldEqual (getStores steps) [Object.singleton "foo" "++, age" # Object.insert "bar" "id"]

  test "can make a migration with both versions and tables and indexes" do
    let
      (Migration m) = defineMigration "mydb"
        # addTable _foo_ (outbound incrementing)
        # addTable _bar_ (inbound nonIncrementing (index _id_))
        # newVersion d1
        # addIndex _foo_ notUnique (index _age_)

      steps = NonEmpty.oneOf m.steps

    Assert.shouldEqual (getVersions steps) [0, 1]
    Assert.shouldEqual (getStores steps) $
      [ Object.singleton "foo" "++" # Object.insert "bar" "id"
      , Object.singleton "foo" "++, age"
      ]
