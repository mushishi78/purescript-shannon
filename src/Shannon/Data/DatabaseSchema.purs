module Shannon.Data.DatabaseSchema where

import Prim.RowList (Cons, Nil) as RowList
import Prim.RowList (class RowToList, RowList)
import Shannon.Data.TableSchema (class TableSchema)


{-| A Row where every value is a table schema
-}
class DatabaseSchema :: forall k. Row k -> Constraint
class DatabaseSchema row

instance databaseSchema
  :: ( RowToList row rowList, DatabaseSchemaRowList rowList )
  => DatabaseSchema row


{-| A RowList where every value is a table schema
-}
class DatabaseSchemaRowList :: forall k. RowList k -> Constraint
class DatabaseSchemaRowList rowList

instance databaseSchemaRowListCons
  :: ( DatabaseSchemaRowList tail, TableSchema v )
  => DatabaseSchemaRowList (RowList.Cons k v tail)

instance databaseSchemaRowListNil :: DatabaseSchemaRowList RowList.Nil

