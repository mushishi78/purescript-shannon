module Shannon where

import Prelude

import Shannon.Data (type (#), Database(..), InboundPrimaryKey, Incrementing, Index, NonIncrementing, NotUnique, OutboundPrimaryKey, WithIndex)
import Shannon.Insert (insertRecord)
import Shannon.Migrate (Migration, defineMigration, newVersion)
import Type.Data.Peano (D1, d1)
import Type.Proxy (Proxy(..))

_foo_ = Proxy :: Proxy "foo"
_bar_ = Proxy :: Proxy "bar"

type MySchema =
  ( foo :: OutboundPrimaryKey NonIncrementing # WithIndex NotUnique (Index "id")
  , bar :: InboundPrimaryKey Incrementing (Index "id")
  )

db :: Database MySchema
db = Database { mappings: unit }

migration :: Migration D1 ()
migration = defineMigration "mydb"
  # newVersion d1

i1 = insertRecord db _foo_ 1 { id: "name" }
i2 = insertRecord db _bar_ unit { id: 1 }
