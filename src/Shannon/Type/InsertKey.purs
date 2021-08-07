module Shannon.Type.InsertKey where

import Prelude

import Data.Maybe (Maybe)
import Dexie.IndexedValue (class IndexedValue)
import Prim.Row (class Cons) as Row
import Shannon.Data.DatabaseSchema (class DatabaseSchema)
import Shannon.Data.TableSchema (InboundPrimaryKey, Incrementing, TableSchema_, NonIncrementing, OutboundPrimaryKey, WithIndex)

class InsertKeyInDatabaseSchema :: forall k. Row k -> Symbol -> Type -> Constraint
class InsertKeyInDatabaseSchema databaseSchema tableName insertKey | databaseSchema tableName -> insertKey

instance findInsertKeyInDatabaseSchema ::
  ( DatabaseSchema databaseSchema
  , Row.Cons tableName tableSchema otherSchemas databaseSchema
  , InsertKeyInTableSchema tableSchema insertKey
  ) =>
  InsertKeyInDatabaseSchema databaseSchema tableName insertKey

class InsertKeyInTableSchema :: TableSchema_ -> Type -> Constraint
class InsertKeyInTableSchema tableSchema insertKey | tableSchema -> insertKey

instance findInsertKeyInTableSchema_OutboundIncrementing :: InsertKeyInTableSchema (OutboundPrimaryKey Incrementing) (Maybe Int)

instance findInsertKeyInTableSchema_OutboundNonIncrementing ::
  ( IndexedValue v
  ) =>
  InsertKeyInTableSchema (OutboundPrimaryKey NonIncrementing) v

instance findInsertKeyInTableSchema_Inbound :: InsertKeyInTableSchema (InboundPrimaryKey incr index) Unit

instance findInsertKeyInTableSchema_Recursing ::
  ( InsertKeyInTableSchema tail insertKey
  ) =>
  InsertKeyInTableSchema (WithIndex uniq indx tail) insertKey
