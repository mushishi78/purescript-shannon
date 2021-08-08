module Shannon.Type.InsertKey where

import Prelude

import Data.Maybe (Maybe(..))
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

--

class InsertKeyToIndexedValue insertKey indexedValue | insertKey -> indexedValue where
  toIndexedValue :: IndexedValue indexedValue => insertKey -> Maybe indexedValue

-- Claim the indexed value is type Int as just need to pass a Nothing to Dexie
instance inboundToIndexedValue :: InsertKeyToIndexedValue Unit Int where
  toIndexedValue _ = Nothing

else instance incrementingToIndexedValue ::
  ( IndexedValue v
  ) => InsertKeyToIndexedValue (Maybe v) v where
  toIndexedValue v = v

else instance nonIncrementingToIndexedValue ::
  ( IndexedValue v
  ) =>
  InsertKeyToIndexedValue v v where
  toIndexedValue v = Just v

