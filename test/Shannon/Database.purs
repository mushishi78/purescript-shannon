module Test.Shannon.Database where

import Prelude

import Data.Maybe (Maybe(..))
import Shannon.Data.Proxy (incrementing, outbound)
import Shannon.Data.Shannon (withImplicitTransactions)
import Shannon.Database (initializeDatabase)
import Shannon.Insert (insert)
import Shannon.MigrationBuilder (addTable)
import Shannon.MigrationBuilder as MigrationBuilder
import Test.Unit (TestSuite, suite, test)
import Type.Proxy (Proxy(..))

_foo_ = Proxy :: Proxy "foo"
_bar_ = Proxy :: Proxy "bar"

_id_ = Proxy :: Proxy "id"
_age_ = Proxy :: Proxy "age"

databaseTests :: TestSuite
databaseTests = suite "database" do
  test "can initialize a database" do
    db <- initializeDatabase
      ( MigrationBuilder.start "mydb"
          # addTable _foo_ (outbound incrementing)
          # MigrationBuilder.end
      )

    withImplicitTransactions db do
      insert _foo_ (Just 1) { id: "name" }

    -- TODO assert that there's data in the table

