module Shannon.Data where

import Prim.RowList (Cons, Nil) as RowList
import Prim.RowList (class RowToList, RowList)
import Type.Function (FLIP)

infixl 0 type FLIP as #

data TableSchema_
foreign import data OutboundPrimaryKey :: Incrementing_ -> TableSchema_
foreign import data InboundPrimaryKey :: Incrementing_ -> Index_ -> TableSchema_
foreign import data WithIndex :: Uniqueness_ -> Index_ -> TableSchema_ -> TableSchema_

class TableSchema (a :: TableSchema_)
instance outboundPrimaryKeyTableSchema :: TableSchema (OutboundPrimaryKey incr)
instance inboundPrimaryKeyTableSchema :: TableSchema (InboundPrimaryKey incr indx)
instance withIndexTableSchema :: TableSchema (WithIndex uniq indx schema)

data Incrementing_
foreign import data Incrementing :: Incrementing_
foreign import data NonIncrementing :: Incrementing_

data Uniqueness_
foreign import data Unique :: Uniqueness_
foreign import data NotUnique :: Uniqueness_

data Index_
foreign import data Index :: Symbol -> Index_
foreign import data CompoundIndex :: Symbol -> Index_ -> Index_

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