module Shannon.Insert where

import Prelude

import Control.Monad.Reader (ReaderT(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Dexie.DB as DB
import Dexie.IndexedValue (class IndexedValue)
import Dexie.Table as Table
import Dexie.Transaction as Transaction
import Shannon.Data.Database (Database(..))
import Shannon.Data.DatabaseSchema (class DatabaseSchema)
import Shannon.Data.Shannon (Shannon)
import Shannon.Type.InsertKey (class InsertKeyInDatabaseSchema, class InsertKeyToIndexedValue, toIndexedValue)
import Type.Proxy (Proxy(..))

insert ::
  forall dbSchema tableName insertKey indexedValue insertValue.
  IsSymbol tableName =>
  DatabaseSchema dbSchema =>
  InsertKeyInDatabaseSchema dbSchema tableName insertKey =>
  InsertKeyToIndexedValue insertKey indexedValue =>
  IndexedValue indexedValue =>
  Proxy tableName ->
  insertKey ->
  insertValue ->
  Shannon dbSchema Unit
insert _ insertKey insertValue = ReaderT \(Database database) -> do
  let tableName = reflectSymbol (Proxy :: Proxy tableName)

  -- Get Table instance from transaction if preset
  table <- case database.trnx of
    Just t -> Transaction.table tableName t
    Nothing -> DB.table tableName database.db

  -- Call .put method for the value
  Table.put insertValue (toIndexedValue insertKey) table # void
