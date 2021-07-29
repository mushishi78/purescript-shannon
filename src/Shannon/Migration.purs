module Shannon.Migration where

import Prelude

import Data.Maybe (Maybe(..))
import Data.NonEmpty (singleton) as NonEmpty
import Data.Symbol (class IsSymbol, reflectSymbol)
import Foreign.Object as Object
import Prim.Ordering (LT)
import Prim.Row (class Cons)
import Record as Record
import Shannon.Data.DatabaseSchema (class DatabaseSchema)
import Shannon.Data.Migration (Migration(..))
import Shannon.Data.MigrationStep as MigrationStep
import Shannon.Data.MigrationSteps as MigrationSteps
import Shannon.Data.TableSchema (class TableSchemaCons)
import Shannon.Symbol (_steps_, _stores_)
import Shannon.Type.MustNotHaveTable (class MustNotHaveTable)
import Shannon.Type.SerializeSchema (class SerializeTableSchema, serializeTableSchema)
import Type.Data.Peano.Nat (class CompareNat, class IsNat, D0, reflectNat)
import Type.Proxy (Proxy(..))

defineMigration :: String -> Migration D0 ()
defineMigration dbName = Migration { dbName, steps: NonEmpty.singleton (MigrationStep.empty 0) }

newVersion ::
  forall v1 v2 databaseSchema proxy.
  IsNat v1 =>
  IsNat v2 =>
  CompareNat v1 v2 LT =>
  proxy v2 -> Migration v1 databaseSchema -> Migration v2 databaseSchema
newVersion _ (Migration m) = Migration $ Record.modify _steps_ (MigrationSteps.cons newStep) m
  where
    newStep = MigrationStep.empty $ reflectNat $ (Proxy :: Proxy v2)

addTable ::
  forall tableName tableSchema version currentSchema mergedSchema.
  IsSymbol tableName =>
  Cons tableName tableSchema currentSchema mergedSchema =>
  DatabaseSchema currentSchema =>
  SerializeTableSchema tableSchema =>
  MustNotHaveTable tableName currentSchema =>
  Proxy tableName -> Proxy tableSchema -> Migration version currentSchema -> Migration version mergedSchema
addTable tableName tableSchema (Migration m) = Migration $ updateSteps m
  where
    updateSteps = Record.modify _steps_ $ MigrationSteps.mapHead $ Record.modify _stores_ updateStores
    updateStores = Object.insert
      (reflectSymbol tableName)
      (serializeTableSchema tableSchema)

addIndex ::
  forall
    version
    previousDatabaseSchema
    currentDatabaseSchema
    newDatabaseSchema
    tableName
    currentTableSchema
    newTableSchema
    uniqueness
    index.
  IsSymbol tableName =>
  Cons tableName currentTableSchema previousDatabaseSchema currentDatabaseSchema =>
  Cons tableName newTableSchema previousDatabaseSchema newDatabaseSchema =>
  DatabaseSchema currentDatabaseSchema =>
  TableSchemaCons uniqueness index currentTableSchema newTableSchema =>
  SerializeTableSchema newTableSchema =>
  Proxy tableName -> Proxy uniqueness -> Proxy index -> Migration version currentDatabaseSchema -> Migration version newDatabaseSchema
addIndex tableName _ _ (Migration m) = Migration $ updateSteps m
  where
    updateSteps = Record.modify _steps_ $ MigrationSteps.mapHead $ Record.modify _stores_ updateStores
    updateStores = Object.update updateStore (reflectSymbol tableName)
    updateStore = const $ Just $ serializeTableSchema (Proxy :: Proxy newTableSchema)