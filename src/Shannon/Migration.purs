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
import Shannon.Data.Migration (AlreadyHasUpgrade, CanUpgrade, CannotUpgradeInitially, Migration(..))
import Shannon.Data.MigrationStep as MigrationStep
import Shannon.Data.MigrationSteps as MigrationSteps
import Shannon.Data.Shannon (Shannon)
import Shannon.Data.TableSchema (class TableSchemaCons)
import Shannon.Symbol (_steps_, _stores_, _upgrade_)
import Shannon.Type.CanChangeStores (class CanChangeStores)
import Shannon.Type.MustHaveTable (class MustHaveTable)
import Shannon.Type.MustNotHaveTable (class MustNotHaveTable)
import Shannon.Type.SerializeSchema (class SerializeTableSchema, serializeTableSchema)
import Shannon.Type.TableSchemaWithoutIndex (class TableSchemaWithoutIndex)
import Type.Data.Peano.Nat (class CompareNat, class IsNat, D0, reflectNat)
import Type.Proxy (Proxy(..))

defineMigration :: String -> Migration D0 () CannotUpgradeInitially
defineMigration dbName = Migration { dbName, steps: MigrationSteps.empty }

newVersion ::
  forall v1 v2 databaseSchema proxy upgradable.
  IsNat v1 =>
  IsNat v2 =>
  CompareNat v1 v2 LT =>
  proxy v2 -> Migration v1 databaseSchema upgradable -> Migration v2 databaseSchema CanUpgrade
newVersion _ (Migration m) = Migration $ Record.modify _steps_ (MigrationSteps.cons newStep) m
  where
    newStep = MigrationStep.empty $ reflectNat $ (Proxy :: Proxy v2)

addTable ::
  forall tableName tableSchema version currentSchema mergedSchema mergedNubbedSchema upgradable.
  IsSymbol tableName =>
  Cons tableName tableSchema currentSchema mergedSchema =>
  Nub mergedSchema mergedNubbedSchema =>
  DatabaseSchema currentSchema =>
  SerializeTableSchema tableSchema =>
  MustNotHaveTable tableName currentSchema =>
  CanChangeStores upgradable =>
  Proxy tableName -> Proxy tableSchema -> Migration version currentSchema upgradable -> Migration version mergedNubbedSchema upgradable
addTable tableName tableSchema (Migration m) = Migration $ updateSteps m
  where
    updateSteps = Record.modify _steps_ $ MigrationSteps.mapHead $ Record.modify _stores_ updateStores
    updateStores = Object.insert (reflectSymbol tableName) newTableSchema
    newTableSchema = Just $ serializeTableSchema tableSchema

removeTable ::
  forall tableName tableSchema version currentSchema newSchema upgradable.
  IsSymbol tableName =>
  Cons tableName tableSchema newSchema currentSchema =>
  DatabaseSchema currentSchema =>
  MustHaveTable tableName currentSchema =>
  CanChangeStores upgradable =>
  Proxy tableName -> Migration version currentSchema upgradable -> Migration version newSchema upgradable
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
    upgradable
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
  CanChangeStores upgradable =>
  Proxy tableName -> Proxy uniqueness -> Proxy index -> Migration version currentDatabaseSchema upgradable -> Migration version newDatabaseSchema upgradable
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
    upgradable
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
  CanChangeStores upgradable =>
  Proxy tableName -> Proxy index -> Migration version currentDatabaseSchema upgradable -> Migration version newDatabaseSchema upgradable
removeIndex tableName _ (Migration m) = Migration $ updateSteps m
  where
    updateSteps = Record.modify _steps_ $ MigrationSteps.mapHead $ Record.modify _stores_ updateStores
    updateStores = Object.insert (reflectSymbol tableName) newTableSchema
    newTableSchema = Just $ serializeTableSchema (Proxy :: Proxy newTableSchema)

setUpgrade :: forall version databaseSchema.
  DatabaseSchema databaseSchema =>
  Shannon databaseSchema Unit -> Migration version databaseSchema CanUpgrade -> Migration version databaseSchema AlreadyHasUpgrade
setUpgrade _ (Migration m) = Migration $ updateSteps m
  where
    updateSteps = Record.modify _steps_ $ MigrationSteps.mapHead $ Record.set _upgrade_ newUpgrade
    newUpgrade = Just $ Promise.resolve unit -- TODO
