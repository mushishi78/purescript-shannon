module Test.Shannon.Type.SerializeSchema  where

import Prelude

import Foreign.Object as Object
import Shannon.Data (type (#), InboundPrimaryKey, Incrementing, Index, NonIncrementing, OutboundPrimaryKey, Unique, WithIndex, compoundIndex, inbound, incrementing, index, nonIncrementing, notUnique, outbound, unique, withIndex)
import Shannon.Type.SerializeSchema (serializeDatabaseSchema, serializeTableSchema)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Type.Proxy (Proxy(..))

_id_ = Proxy :: Proxy "id"
_name_ = Proxy :: Proxy "name"
_age_ = Proxy :: Proxy "age"
_height_ = Proxy :: Proxy "height"
_parent_ = Proxy :: Proxy "parent"

serializeSchemaTests :: TestSuite
serializeSchemaTests = suite "serializeSchema" do
  test "serializes an outbound nonIncrementing table" do
    let tableSchema = outbound nonIncrementing
    Assert.shouldEqual (serializeTableSchema tableSchema) ""

  test "serializes an outbound incrementing table" do
    let tableSchema = outbound incrementing
    Assert.shouldEqual (serializeTableSchema tableSchema) "++"

  test "serializes an inbound nonIncrementing table" do
    let tableSchema = inbound nonIncrementing (index _id_)
    Assert.shouldEqual (serializeTableSchema tableSchema) "id"

  test "serializes an inbound incrementing table" do
    let tableSchema = inbound incrementing (index _id_)
    Assert.shouldEqual (serializeTableSchema tableSchema) "++id"

  test "serializes an inbound nonIncrementing table with compound primary key" do
    let tableSchema = inbound nonIncrementing (index _id_ # compoundIndex _name_ # compoundIndex _age_)
    Assert.shouldEqual (serializeTableSchema tableSchema) "[id+name+age]"

  test "serializes an outbound nonIncrementing table with secondary indexes" do
    let tableSchema =
          outbound nonIncrementing
            # withIndex notUnique (index _name_)
            # withIndex notUnique (index _age_)
    Assert.shouldEqual (serializeTableSchema tableSchema) ", name, age"

  test "serializes an inbound nonIncrementing table with secondary indexes" do
    let tableSchema =
          inbound nonIncrementing (index _id_)
            # withIndex notUnique (index _name_)
            # withIndex notUnique (index _age_)
    Assert.shouldEqual (serializeTableSchema tableSchema) "id, name, age"

  test "serializes a table with unique secondary indexes" do
    let tableSchema =
          inbound nonIncrementing (index _id_)
            # withIndex unique (index _name_)
            # withIndex unique (index _age_)
    Assert.shouldEqual (serializeTableSchema tableSchema) "id, &name, &age"

  test "serializes a table with compound secondary indexes" do
    let tableSchema =
          inbound nonIncrementing (index _id_)
            # withIndex notUnique (index _id_ # compoundIndex _name_ # compoundIndex _age_)
            # withIndex unique (index _height_ # compoundIndex _parent_)
    Assert.shouldEqual (serializeTableSchema tableSchema) "id, [id+name+age], &[height+parent]"

  test "serializes a database schema" do
    let
        databaseSchema :: Proxy
            ( foo :: OutboundPrimaryKey Incrementing # WithIndex Unique (Index "age")
            , bar :: InboundPrimaryKey NonIncrementing (Index "id")
            )
        databaseSchema = Proxy

        expected = Object.singleton "foo" "++, &age" # Object.insert "bar" "id"

    Assert.shouldEqual (serializeDatabaseSchema databaseSchema) expected