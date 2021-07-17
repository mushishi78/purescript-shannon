module Shannon.Insert where

import Prelude

import Data.Symbol (class IsSymbol)
import Dexie.Promise (Promise)
import Dexie.Promise as Promise
import Shannon.Data (Database)
import Shannon.Type.InsertKey (class InsertKeyInDatabaseSchema)
import Type.Proxy (Proxy)

insertRecord ::
  forall databaseSchema tableName insertKey insertRow.
    IsSymbol tableName =>
    InsertKeyInDatabaseSchema databaseSchema tableName insertKey =>
    Database databaseSchema -> Proxy tableName -> insertKey -> Record insertRow -> Promise Unit
insertRecord _ _ _ _ = Promise.resolve unit
