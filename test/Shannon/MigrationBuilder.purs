module Test.Shannon.MigrationBuilder where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), isJust)
import Data.NonEmpty as NonEmpty
import Foreign.Object as Object
import Shannon.Data.MigrationBuilder (getDBName, getSteps)
import Shannon.Data.Proxy (compoundIndex, inbound, incrementing, index, nonIncrementing, notUnique, outbound)
import Shannon.Insert (insert)
import Shannon.MigrationBuilder (addIndex, addTable, newVersion, removeIndex, removeTable, setUpgrade)
import Shannon.MigrationBuilder as MigrationBuilder
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Type.Data.Peano (d2, d5)
import Type.Proxy (Proxy(..))

_foo_ = Proxy :: Proxy "foo"
_bar_ = Proxy :: Proxy "bar"

_id_ = Proxy :: Proxy "id"
_age_ = Proxy :: Proxy "age"

migrationTests :: TestSuite
migrationTests = suite "migration" do
  let
    getVersions = map (_.version) >>> Array.reverse
    getStores = map (_.stores) >>> Array.reverse

  test "can make an empty migration" do
    let
      steps = NonEmpty.oneOf $ getSteps migration
      migration = MigrationBuilder.start "mydb"

    Assert.shouldEqual (getDBName migration) "mydb"
    Assert.shouldEqual (getVersions steps) [ 0 ]
    Assert.shouldEqual (getStores steps) [ Object.empty ]

  test "can make a migration with some version changes" do
    let
      steps = NonEmpty.oneOf $ getSteps migration
      migration = MigrationBuilder.start "mydb"
        # newVersion d2
        # newVersion d5

    Assert.shouldEqual (getVersions steps) [ 0, 1, 5 ]
    Assert.shouldEqual (getStores steps) [ Object.empty, Object.empty, Object.empty ]

  test "can make a migration with some tables and indexes" do
    let
      steps = NonEmpty.oneOf $ getSteps migration
      migration = MigrationBuilder.start "mydb"
        # addTable _foo_ (outbound incrementing)
        # addTable _bar_ (inbound nonIncrementing (index _id_))
        # addIndex _foo_ notUnique (index _age_)

    Assert.shouldEqual (getVersions steps) [ 0 ]
    Assert.shouldEqual (getStores steps)
      [ Object.singleton "foo" (Just "++, age") # Object.insert "bar" (Just "id")
      ]

  test "can make a migration with both versions and tables and indexes" do
    let
      steps = NonEmpty.oneOf $ getSteps migration
      migration = MigrationBuilder.start "mydb"
        # addTable _foo_ (outbound incrementing)
        # addTable _bar_ (inbound nonIncrementing (index _id_))
        # newVersion d2
        # addIndex _foo_ notUnique (index _age_)

    Assert.shouldEqual (getVersions steps) [ 0, 1 ]
    Assert.shouldEqual (getStores steps) $
      [ Object.singleton "foo" (Just "++") # Object.insert "bar" (Just "id")
      , Object.singleton "foo" (Just "++, age")
      ]

  test "can remove an index from the end of a table schema" do
    let
      steps = NonEmpty.oneOf $ getSteps migration
      migration = MigrationBuilder.start "mydb"
        # addTable _foo_ (outbound incrementing)
        # addTable _bar_ (inbound nonIncrementing (index _id_))
        # addIndex _foo_ notUnique (index _age_)
        # newVersion d2
        # removeIndex _foo_ (index _age_)

    Assert.shouldEqual (getVersions steps) [ 0, 1 ]
    Assert.shouldEqual (getStores steps) $
      [ Object.singleton "foo" (Just "++, age") # Object.insert "bar" (Just "id")
      , Object.singleton "foo" (Just "++")
      ]

  test "can remove an index from the middle of a table schema" do
    let
      steps = NonEmpty.oneOf $ getSteps migration
      migration = MigrationBuilder.start "mydb"
        # addTable _foo_ (outbound incrementing)
        # addTable _bar_ (inbound nonIncrementing (index _id_))
        # addIndex _foo_ notUnique (index _age_)
        # addIndex _foo_ notUnique (compoundIndex _id_ $ index _age_)
        # newVersion d2
        # removeIndex _foo_ (index _age_)

    Assert.shouldEqual (getVersions steps) [ 0, 1 ]
    Assert.shouldEqual (getStores steps) $
      [ Object.singleton "foo" (Just "++, age, [age+id]") # Object.insert "bar" (Just "id")
      , Object.singleton "foo" (Just "++, [age+id]")
      ]

  test "can remove a table" do
    let
      steps = NonEmpty.oneOf $ getSteps migration
      migration = MigrationBuilder.start "mydb"
        # addTable _foo_ (outbound incrementing)
        # addTable _bar_ (inbound nonIncrementing (index _id_))
        # newVersion d2
        # removeTable _foo_

    Assert.shouldEqual (getVersions steps) [ 0, 1 ]
    Assert.shouldEqual (getStores steps) $
      [ Object.singleton "foo" (Just "++") # Object.insert "bar" (Just "id")
      , Object.singleton "foo" Nothing
      ]

  test "can set upgrade effect for migration" do
    let
      steps = NonEmpty.oneOf $ getSteps migration
      migration = MigrationBuilder.start "mydb"
        # addTable _foo_ (outbound incrementing)
        # addTable _bar_ (inbound nonIncrementing (index _id_))
        # newVersion d2
        # setUpgrade
          ( do
              insert _foo_ (Just 1) { id: "name" }
              insert _bar_ unit { id: 1 }
          )

      upgradesAreJust = map (_.upgrade >>> isJust) steps # Array.reverse

    Assert.shouldEqual (getVersions steps) [ 0, 1 ]
    Assert.shouldEqual upgradesAreJust [ false, true ]