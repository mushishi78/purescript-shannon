module Shannon.Data where

import Prim.RowList (Cons, Nil) as RowList
import Prim.RowList (class RowToList, RowList)
import Type.Data.Peano.Nat (class IsNat, Nat)
import Type.Function (FLIP)
import Type.Proxy (Proxy)

infixl 0 type FLIP as #

type Database :: forall k. Nat -> Row k -> Type
type Database version databaseSchema =
    IsNat version =>
    DatabaseSchema databaseSchema =>
    { version :: Proxy version
    , schema :: Proxy databaseSchema
    }

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
