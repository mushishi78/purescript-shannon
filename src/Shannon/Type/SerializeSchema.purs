module Shannon.Type.SerializeSchema where

import Prelude

import Data.Symbol (class IsSymbol, reflectSymbol)
import Foreign.Object (Object)
import Foreign.Object as Object
import Prim.RowList (Cons, Nil) as RowList
import Prim.RowList (class RowToList, RowList)
import Shannon.Data.TableSchema (CompoundIndex, InboundPrimaryKey, Incrementing, Incrementing_, Index, Index_, NonIncrementing, NotUnique, OutboundPrimaryKey, TableSchema_, Unique, Uniqueness_, WithIndex)
import Type.Proxy (Proxy(..))

class SerializeDatabaseSchema :: forall r. Row r -> Constraint
class SerializeDatabaseSchema databaseSchema where
    serializeDatabaseSchema :: Proxy databaseSchema -> Object String

instance serializeDatabaseSchemaRow
    :: ( RowToList row rowList, SerializeDatabaseSchemaRowList rowList )
    => SerializeDatabaseSchema row where
  serializeDatabaseSchema _ = serializeDatabaseSchemaRowList (Proxy :: Proxy rowList)

--

class SerializeDatabaseSchemaRowList :: forall r. RowList r -> Constraint
class SerializeDatabaseSchemaRowList databaseSchema where
    serializeDatabaseSchemaRowList :: Proxy databaseSchema -> Object String

instance serializeDatabaseSchemaRowList_Cons
    :: ( IsSymbol k, SerializeTableSchema v, SerializeDatabaseSchemaRowList tail )
    => SerializeDatabaseSchemaRowList (RowList.Cons k v tail) where
  serializeDatabaseSchemaRowList _ =
    Object.insert
        (reflectSymbol (Proxy :: Proxy k))
        (serializeTableSchema (Proxy :: Proxy v))
        (serializeDatabaseSchemaRowList (Proxy :: Proxy tail))

else instance serializeDatabaseSchemaRowList_Nil
    :: SerializeDatabaseSchemaRowList RowList.Nil where
  serializeDatabaseSchemaRowList _ = Object.empty

--

class SerializeTableSchema (tableSchema :: TableSchema_) where
  serializeTableSchema :: Proxy tableSchema -> String

instance serializeTableSchema_OutboundPrimaryKey
    :: SerializeIncrementing incr
    => SerializeTableSchema (OutboundPrimaryKey incr) where
  serializeTableSchema _ = serializeIncrementing (Proxy :: Proxy incr)

instance serializeTableSchema_InboundPrimaryKey
    :: (SerializeIncrementing incr, SerializeIndex indx)
    => SerializeTableSchema (InboundPrimaryKey incr indx) where
  serializeTableSchema _ = serializeIncrementing (Proxy :: Proxy incr) <> serializeIndex (Proxy :: Proxy indx)

instance serializeTableSchema_WithIndex
    :: (SerializeUniqueness uniq, SerializeIndex indx, SerializeTableSchema tail)
    => SerializeTableSchema (WithIndex uniq indx tail) where
  serializeTableSchema _ =
    serializeTableSchema (Proxy :: Proxy tail)
    <> ", "
    <> serializeUniqueness (Proxy :: Proxy uniq)
    <> serializeIndex (Proxy :: Proxy indx)

--

class SerializeIncrementing (incr :: Incrementing_) where
  serializeIncrementing :: Proxy incr -> String

instance serializeIncrementing_Incrementing :: SerializeIncrementing Incrementing where
  serializeIncrementing _ = "++"

instance serializeIncrementing_NonIncrementing :: SerializeIncrementing NonIncrementing where
  serializeIncrementing _ = ""

--

class SerializeUniqueness (uniqueness :: Uniqueness_) where
  serializeUniqueness :: Proxy uniqueness -> String

instance serializeUniqueness_Unique :: SerializeUniqueness Unique where
  serializeUniqueness _ = "&"

instance serializeUniqueness_NotUnique :: SerializeUniqueness NotUnique where
  serializeUniqueness _ = ""

--

class SerializeIndex (index :: Index_) where
  serializeIndex :: Proxy index -> String
  serializeIndexMember :: Proxy index -> String

instance serializeIndex_Index :: IsSymbol s => SerializeIndex (Index s) where
  serializeIndex p = serializeIndexMember p
  serializeIndexMember _ = reflectSymbol (Proxy :: Proxy s)

instance serializeIndex_CompoundIndex :: (IsSymbol s, SerializeIndex i) => SerializeIndex (CompoundIndex s i) where
  serializeIndex p = "[" <> (serializeIndexMember p) <> "]"
  serializeIndexMember _ = serializeIndexMember (Proxy :: Proxy i) <> "+" <> reflectSymbol (Proxy :: Proxy s)

--
