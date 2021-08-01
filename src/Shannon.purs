module Shannon where

import Prelude

import Effect.Aff (Aff)
import Shannon.Data.Database (Database(..))
import Shannon.Data.Migration (CanUpgrade, Migration)
import Shannon.Data.Proxy (inbound, incrementing, index, nonIncrementing, notUnique, outbound)
import Shannon.Data.Shannon (withImplicitTransactions)
import Shannon.Data.TableSchema (type (#), InboundPrimaryKey, Incrementing, Index, NonIncrementing, NotUnique, OutboundPrimaryKey, WithIndex)
import Shannon.Insert (insert)
import Shannon.Migration (addIndex, addTable, defineMigration, newVersion)
import Type.Data.Peano (D1, d1)
import Type.Proxy (Proxy(..))

_foo_ = Proxy :: Proxy "foo"
_bar_ = Proxy :: Proxy "bar"
_id_ = Proxy :: Proxy "id"

type MySchema =
  ( foo :: OutboundPrimaryKey NonIncrementing # WithIndex NotUnique (Index "id")
  , bar :: InboundPrimaryKey Incrementing (Index "id")
  )

db :: Database MySchema
db = Database { mappings: unit }

migration :: Migration D1 MySchema CanUpgrade
migration = defineMigration "mydb"
  # addTable _foo_ (outbound nonIncrementing)
  # newVersion d1
  # addTable _bar_ (inbound incrementing (index _id_))
  # addIndex _foo_ notUnique (index _id_)

insertExample :: Aff Unit
insertExample = withImplicitTransactions db do
  insert _foo_ 1 { id: "name" }
  insert _bar_ unit { id: 1 }

