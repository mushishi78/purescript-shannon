module Shannon.Data where

import Prelude

import Prim.RowList (Cons, Nil) as RowList
import Prim.RowList (class RowToList, RowList)
import Type.Function (FLIP)
import Type.Proxy (Proxy(..))

infixl 0 type FLIP as #

data Database :: forall k. Row k -> Type
data Database databaseSchema = Database (
  DatabaseSchema databaseSchema =>
    { mappings :: Unit
    }
  )
--

class DatabaseSchema :: forall k. Row k -> Constraint
class DatabaseSchema row

instance databaseSchema
  :: ( RowToList row rowList, DatabaseSchemaRowList rowList )
  => DatabaseSchema row

--

class DatabaseSchemaRowList :: forall k. RowList k -> Constraint
class DatabaseSchemaRowList rowList

instance databaseSchemaRowListCons
  :: ( DatabaseSchemaRowList tail, TableSchema v)
  => DatabaseSchemaRowList (RowList.Cons k v tail)

instance databaseSchemaRowListNil :: DatabaseSchemaRowList RowList.Nil

--

data TableSchema_
foreign import data OutboundPrimaryKey :: Incrementing_ -> TableSchema_
foreign import data InboundPrimaryKey :: Incrementing_ -> Index_ -> TableSchema_
foreign import data WithIndex :: Uniqueness_ -> Index_ -> TableSchema_ -> TableSchema_

class TableSchema (ts :: TableSchema_)
instance tableSchema_OutboundPrimaryKey :: TableSchema (OutboundPrimaryKey incr)
instance tableSchema_InboundPrimaryKey_Incrementing :: TableSchema (InboundPrimaryKey Incrementing (Index s))
instance tableSchema_InboundPrimaryKey_NonIncrementing :: TableSchema (InboundPrimaryKey NonIncrementing indx)
instance tableSchema_WithIndex :: TableSchema (WithIndex uniq indx schema)

data Incrementing_
foreign import data Incrementing :: Incrementing_
foreign import data NonIncrementing :: Incrementing_

data Uniqueness_
foreign import data Unique :: Uniqueness_
foreign import data NotUnique :: Uniqueness_

data Index_
foreign import data Index :: Symbol -> Index_
foreign import data CompoundIndex :: Symbol -> Index_ -> Index_


outbound :: forall incr. Proxy incr -> Proxy (OutboundPrimaryKey incr)
outbound _ = Proxy

inbound :: forall incr indx. Proxy incr -> Proxy indx -> Proxy (InboundPrimaryKey incr indx)
inbound _ _ = Proxy

withIndex :: forall uniq indx tail. Proxy uniq -> Proxy indx -> Proxy tail -> Proxy (WithIndex uniq indx tail)
withIndex _ _ _ = Proxy

incrementing = Proxy :: Proxy Incrementing
nonIncrementing = Proxy :: Proxy NonIncrementing
unique = Proxy :: Proxy Unique
notUnique = Proxy :: Proxy NotUnique

index :: forall sym. Proxy sym -> Proxy (Index sym)
index _ = Proxy

compoundIndex :: forall sym indx. Proxy sym -> Proxy indx -> Proxy (CompoundIndex sym indx)
compoundIndex _ _ = Proxy
