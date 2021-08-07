module Shannon.Type.MustHaveTable where

import Data.Symbol (class IsSymbol)
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Prim.TypeError (class Fail, Beside, Text)

infixl 4 type Beside as <>

--

class MustHaveTable :: forall k. Symbol -> Row k -> Constraint
class MustHaveTable tableName databaseSchema

instance mustNotHaveTable ::
  ( RowToList databaseSchema databaseSchemaRowList
  , MustHaveTableRowList tableName databaseSchemaRowList
  ) =>
  MustHaveTable tableName databaseSchema

--

class MustHaveTableRowList :: forall k. Symbol -> RowList k -> Constraint
class MustHaveTableRowList tableName databaseSchemaRowList

instance mustLackRowList_Fail ::
  ( Fail (Text "Must have table called " <> Text tableName <> Text " in database")
  , IsSymbol tableName
  ) =>
  MustHaveTableRowList tableName Nil

else instance mustLackRowList_Found :: MustHaveTableRowList tableName (Cons tableName tableSchema tail)

else instance mustLackRowList_Looking ::
  ( MustHaveTableRowList tableName tail
  ) =>
  MustHaveTableRowList tableName (Cons otherTableName otherTableSchema tail)

