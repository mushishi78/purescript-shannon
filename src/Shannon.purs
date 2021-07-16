module Shannon where

import Prelude

import Shannon.Data (class IndexSchemaRow, InboundPrimaryKey, Incrementing, Index, NonIncrementing, NotUnique, OutboundPrimaryKey, WithIndex)
import Shannon.Indexed.Insert (insertRecord)
import Type.Proxy (Proxy(..))

--

_id_ = Proxy :: Proxy "id"
_age_ = Proxy :: Proxy "age"
_name_ = Proxy :: Proxy "name"

_foo_ = Proxy :: Proxy "foo"
_bar_ = Proxy :: Proxy "bar"

schema = Proxy :: Proxy Schema

type Schema =
  ( foo :: WithIndex NotUnique (Index "id") (OutboundPrimaryKey NonIncrementing)
  , bar :: InboundPrimaryKey Incrementing (Index "id")
  )

migration :: forall r. IndexSchemaRow r => Proxy r -> Unit
migration _ = unit

m :: Unit
m = migration schema


i1 = insertRecord schema _foo_ 1 { id: "name" }
i2 = insertRecord schema _bar_ unit { id: 1 }


