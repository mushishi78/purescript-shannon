module Shannon.Type.TableSchemaWithoutIndex where

import Prim.TypeError (class Fail, Text)
import Shannon.Data.TableSchema (InboundPrimaryKey, Index_, OutboundPrimaryKey, TableSchema_, WithIndex)

class TableSchemaWithoutIndex :: Index_ -> TableSchema_ -> TableSchema_ -> Constraint
class TableSchemaWithoutIndex index tableSchemaWith tableSchemaWithout | index tableSchemaWith -> tableSchemaWithout 

instance tableSchemaWithoutIndex_remove
    :: TableSchemaWithoutIndex index (WithIndex uniq index without) without

else instance tableSchemaWithoutIndex_looking
    :: (TableSchemaWithoutIndex index tail newTail)
    => TableSchemaWithoutIndex index (WithIndex uniq otherIndex tail) (WithIndex uniq otherIndex newTail)

else instance tableSchemaWithoutIndex_inbound
    :: (Fail (Text "Could not find index in schema"))
    => TableSchemaWithoutIndex index (InboundPrimaryKey incr index) nothing

else instance tableSchemaWithoutIndex_outbound
    :: (Fail (Text "Could not find index in schema"))
    => TableSchemaWithoutIndex index (OutboundPrimaryKey incr) nothing
