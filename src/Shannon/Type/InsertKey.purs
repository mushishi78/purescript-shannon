module Shannon.Type.InsertKey where

import Prelude

import Dexie.IndexedValue (class IndexedValue)
import Prim.Row (class Cons) as Row
import Shannon.Data (class IndexSchemaRow, InboundPrimaryKey, Incrementing, IndexSchema_, NonIncrementing, OutboundPrimaryKey, WithIndex)

class InsertKeyInIndexSchemaRow :: forall k. Row k -> Symbol -> Type -> Constraint
class InsertKeyInIndexSchemaRow schemaRow tableName insertKey | schemaRow tableName -> insertKey

instance findInsertKeyInIndexSchemaRow
  :: ( IndexSchemaRow schemaRow
     , Row.Cons tableName schema otherSchemas schemaRow
     , InsertKeyInIndexSchema schema insertKey
     )
  => InsertKeyInIndexSchemaRow schemaRow tableName insertKey

class InsertKeyInIndexSchema :: IndexSchema_ -> Type -> Constraint
class InsertKeyInIndexSchema schema insertKey | schema -> insertKey

instance findInsertKeyInIndexSchema_OutboundIncrementing
  :: InsertKeyInIndexSchema (OutboundPrimaryKey Incrementing) Int

instance findInsertKeyInIndexSchema_OutboundNonIncrementing
  :: (IndexedValue v)
  => InsertKeyInIndexSchema (OutboundPrimaryKey NonIncrementing) v

instance findInsertKeyInIndexSchema_Inbound
  :: InsertKeyInIndexSchema (InboundPrimaryKey incr index) Unit

instance findInsertKeyInIndexSchema_Recursing
  :: (InsertKeyInIndexSchema tail insertKey)
  => InsertKeyInIndexSchema (WithIndex uniq indx tail) insertKey
