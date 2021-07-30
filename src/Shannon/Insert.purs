module Shannon.Insert where

import Prelude

import Data.Symbol (class IsSymbol)
import Dexie.Promise (Promise)
import Dexie.Promise as Promise
import Shannon.Data.Database (Database)
import Shannon.Type.InsertKey (class InsertKeyInDatabaseSchema)
import Type.Proxy (Proxy)

insert ::
  forall dbSchema tableName insertKey insertRow.
    IsSymbol tableName =>
    InsertKeyInDatabaseSchema dbSchema tableName insertKey =>
    Database dbSchema -> Proxy tableName -> insertKey -> Record insertRow -> Promise Unit
insert _ _ _ _ = Promise.resolve unit
