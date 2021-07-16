module Shannon.Indexed.Insert where

import Prelude

import Data.Symbol (class IsSymbol)
import Dexie.Promise (Promise)
import Dexie.Promise as Promise
import Shannon.Data (class IndexSchemaRow)
import Shannon.Type.InsertKey (class InsertKeyInIndexSchemaRow)
import Type.Proxy (Proxy)

insertRecord ::
  forall schemaRow tableName insertKey insertRow.
    IndexSchemaRow schemaRow =>
    IsSymbol tableName =>
    InsertKeyInIndexSchemaRow schemaRow tableName insertKey =>
    Proxy schemaRow -> Proxy tableName -> insertKey -> Record insertRow -> Promise Unit
insertRecord _ _ _ _ = Promise.resolve unit
