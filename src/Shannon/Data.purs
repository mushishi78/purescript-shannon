module Shannon.Data where

import Prim.RowList (Cons, Nil) as RowList
import Prim.RowList (class RowToList, RowList)

data IndexSchema_
foreign import data OutboundPrimaryKey :: Incrementing_ -> IndexSchema_
foreign import data InboundPrimaryKey :: Incrementing_ -> Index_ -> IndexSchema_
foreign import data WithIndex :: Uniqueness_ -> Index_ -> IndexSchema_ -> IndexSchema_

class IndexSchema (a :: IndexSchema_)
instance outboundPrimaryKeyIndexSchema :: IndexSchema (OutboundPrimaryKey incr)
instance inboundPrimaryKeyIndexSchema :: IndexSchema (InboundPrimaryKey incr indx)
instance withIndexIndexSchema :: IndexSchema (WithIndex uniq indx schema)

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

class IndexSchemaRow :: forall k. Row k -> Constraint
class IndexSchemaRow row

instance indexSchemaRow
  :: ( RowToList row rowList, IndexSchemaRowList rowList )
  => IndexSchemaRow row

--

class IndexSchemaRowList :: forall k. RowList k -> Constraint
class IndexSchemaRowList rowList

instance indexSchemaRowListCons
  :: ( IndexSchemaRowList tail, IndexSchema v)
  => IndexSchemaRowList (RowList.Cons k v tail)

instance indexSchemaRowListNil :: IndexSchemaRowList RowList.Nil

--