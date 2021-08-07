module Shannon.Type.MustNotHaveTable where

import Data.Symbol (class IsSymbol)
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Prim.TypeError (class Fail, Beside, Text)

infixl 4 type Beside as <>

--

class MustNotHaveTable :: forall k. Symbol -> Row k -> Constraint
class MustNotHaveTable tableName databaseSchema

instance mustNotHaveTable ::
  ( RowToList databaseSchema databaseSchemaRowList
  , MustNotHaveTableRowList tableName databaseSchemaRowList
  ) =>
  MustNotHaveTable tableName databaseSchema

--

class MustNotHaveTableRowList :: forall k. Symbol -> RowList k -> Constraint
class MustNotHaveTableRowList tableName databaseSchemaRowList

instance mustLackRowList_Fail ::
  ( Fail (Text "Already have table called " <> Text tableName <> Text " in database")
  , IsSymbol tableName
  ) =>
  MustNotHaveTableRowList tableName (Cons tableName tableSchema tail)

else instance mustLackRowList_Looking ::
  ( MustNotHaveTableRowList tableName tail
  ) =>
  MustNotHaveTableRowList tableName (Cons otherTableName otherTableSchema tail)

else instance mustLackRowList_Nil :: MustNotHaveTableRowList tableName Nil
