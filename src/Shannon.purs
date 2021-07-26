module Shannon where

import Prelude

import Shannon.Data.Database (Database(..))
import Shannon.Data.TableSchema (type (#), InboundPrimaryKey, Incrementing, Index, NonIncrementing, NotUnique, OutboundPrimaryKey, WithIndex)
import Shannon.Data.Proxy (inbound, incrementing, nonIncrementing, outbound, index)
import Shannon.Data.Migration (Migration)
import Shannon.Insert (insertRecord)
import Shannon.Migration (defineMigration, newVersion, addTable)
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

migration :: Migration D1 ( foo :: OutboundPrimaryKey NonIncrementing, bar :: InboundPrimaryKey Incrementing (Index "id") )
migration = defineMigration "mydb"
  # addTable _foo_ (outbound nonIncrementing)
  # newVersion d1
  # addTable _bar_ (inbound incrementing (index _id_))

i1 = insertRecord db _foo_ 1 { id: "name" }
i2 = insertRecord db _bar_ unit { id: 1 }
