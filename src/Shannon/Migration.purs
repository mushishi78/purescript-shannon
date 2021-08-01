module Shannon.Migration where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Dexie.Promise as Promise
import Foreign.Object as Object
import Prim.Ordering (LT)
import Prim.Row (class Cons, class Nub)
import Record as Record
import Shannon.Data.DatabaseSchema (class DatabaseSchema)
import Shannon.Data.Migration (Migration(..))
import Shannon.Data.MigrationStep as MigrationStep
import Shannon.Data.MigrationSteps as MigrationSteps
import Shannon.Data.Shannon (Shannon)
import Shannon.Data.TableSchema (class TableSchemaCons)
import Shannon.Symbol (_steps_, _stores_, _upgrade_)
import Shannon.Type.MustHaveTable (class MustHaveTable)
import Shannon.Type.MustNotHaveTable (class MustNotHaveTable)
import Shannon.Type.SerializeSchema (class SerializeTableSchema, serializeTableSchema)
import Shannon.Type.TableSchemaWithoutIndex (class TableSchemaWithoutIndex)
import Type.Data.Peano.Nat (class CompareNat, class IsNat, D0, reflectNat)
import Type.Proxy (Proxy(..))

defineMigration :: String -> Migration D0 ()
defineMigration dbName = Migration { dbName, steps: MigrationSteps.empty }

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
  forall tableName tableSchema version currentSchema mergedSchema mergedNubbedSchema.
  IsSymbol tableName =>
  Cons tableName tableSchema currentSchema mergedSchema =>
  Nub mergedSchema mergedNubbedSchema =>
  DatabaseSchema currentSchema =>
  SerializeTableSchema tableSchema =>
  MustNotHaveTable tableName currentSchema =>
  Proxy tableName -> Proxy tableSchema -> Migration version currentSchema -> Migration version mergedNubbedSchema
addTable tableName tableSchema (Migration m) = Migration $ updateSteps m
  where
    updateSteps = Record.modify _steps_ $ MigrationSteps.mapHead $ Record.modify _stores_ updateStores
    updateStores = Object.insert (reflectSymbol tableName) newTableSchema
    newTableSchema = Just $ serializeTableSchema tableSchema

removeTable ::
  forall tableName tableSchema version currentSchema newSchema.
  IsSymbol tableName =>
  Cons tableName tableSchema newSchema currentSchema =>
  DatabaseSchema currentSchema =>
  MustHaveTable tableName currentSchema =>
  Proxy tableName -> Migration version currentSchema -> Migration version newSchema
removeTable tableName (Migration m) = Migration $ updateSteps m
  where
    updateSteps = Record.modify _steps_ $ MigrationSteps.mapHead $ Record.modify _stores_ updateStores
    updateStores = Object.insert (reflectSymbol tableName) Nothing

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
    updateStores = Object.insert (reflectSymbol tableName) newTableSchema
    newTableSchema = Just $ serializeTableSchema (Proxy :: Proxy newTableSchema)

removeIndex ::
  forall
    version
    previousDatabaseSchema
    currentDatabaseSchema
    newDatabaseSchema
    tableName
    currentTableSchema
    newTableSchema
    index.
  IsSymbol tableName =>
  Cons tableName currentTableSchema previousDatabaseSchema currentDatabaseSchema =>
  Cons tableName newTableSchema previousDatabaseSchema newDatabaseSchema =>
  DatabaseSchema currentDatabaseSchema =>
  TableSchemaWithoutIndex index currentTableSchema newTableSchema =>
  SerializeTableSchema newTableSchema =>
  Proxy tableName -> Proxy index -> Migration version currentDatabaseSchema -> Migration version newDatabaseSchema
removeIndex tableName _ (Migration m) = Migration $ updateSteps m
  where
    updateSteps = Record.modify _steps_ $ MigrationSteps.mapHead $ Record.modify _stores_ updateStores
    updateStores = Object.insert (reflectSymbol tableName) newTableSchema
    newTableSchema = Just $ serializeTableSchema (Proxy :: Proxy newTableSchema)

setUpgrade :: forall version databaseSchema.
  DatabaseSchema databaseSchema =>
  Shannon databaseSchema Unit -> Migration version databaseSchema -> Migration version databaseSchema
setUpgrade _ (Migration m) = Migration $ updateSteps m
  where
    updateSteps = Record.modify _steps_ $ MigrationSteps.mapHead $ Record.set _upgrade_ newUpgrade
    newUpgrade = Just $ Promise.resolve unit -- TODO
