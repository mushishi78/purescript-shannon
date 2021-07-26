module Shannon.Data.TableSchema where

import Type.Function (FLIP)

infixl 0 type FLIP as #

data TableSchema_
foreign import data OutboundPrimaryKey :: Incrementing_ -> TableSchema_
foreign import data InboundPrimaryKey :: Incrementing_ -> Index_ -> TableSchema_
foreign import data WithIndex :: Uniqueness_ -> Index_ -> TableSchema_ -> TableSchema_

class TableSchema (ts :: TableSchema_)
instance tableSchema_OutboundPrimaryKey :: TableSchema (OutboundPrimaryKey incr)
instance tableSchema_InboundPrimaryKey_Incrementing :: TableSchema (InboundPrimaryKey Incrementing (Index s))
instance tableSchema_InboundPrimaryKey_NonIncrementing :: TableSchema (InboundPrimaryKey NonIncrementing indx)
instance tableSchema_WithIndex :: TableSchema (WithIndex uniq indx schema)

class TableSchemaCons :: Uniqueness_ -> Index_ -> TableSchema_ -> TableSchema_ -> Constraint
class TableSchemaCons uniq indx before after | uniq indx before -> after

instance tableSchemaCons :: TableSchemaCons uniq indx schema (WithIndex uniq indx schema)

data Incrementing_
foreign import data Incrementing :: Incrementing_
foreign import data NonIncrementing :: Incrementing_

data Uniqueness_
foreign import data Unique :: Uniqueness_
foreign import data NotUnique :: Uniqueness_

data Index_
foreign import data Index :: Symbol -> Index_
foreign import data CompoundIndex :: Symbol -> Index_ -> Index_
