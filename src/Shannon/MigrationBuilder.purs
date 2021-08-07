module Shannon.MigrationBuilder where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Dexie.Promise as Promise
import Foreign.Object as Object
import Prim.Ordering (LT)
import Prim.Row (class Cons, class Nub)
import Record as Record
import Shannon.Data.DatabaseSchema (class DatabaseSchema)
import Shannon.Data.DefinedMigration (DefinedMigration(..))
import Shannon.Data.MigrationBuilder (AlreadyHasUpgrade, CanUpgrade, CannotUpgradeInitially, MigrationBuilder(..))
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

startMigrationDefinition :: String -> MigrationBuilder D0 () CannotUpgradeInitially
startMigrationDefinition dbName = MigrationBuilder { dbName, steps: MigrationSteps.empty }

newVersion ::
  forall v1 v2 databaseSchema proxy upgradable.
  IsNat v1 =>
  IsNat v2 =>
  CompareNat v1 v2 LT =>
  proxy v2 -> MigrationBuilder v1 databaseSchema upgradable -> MigrationBuilder v2 databaseSchema CanUpgrade
newVersion _ (MigrationBuilder m) = MigrationBuilder $ Record.modify _steps_ (MigrationSteps.cons newStep) m
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
  Proxy tableName -> Proxy tableSchema -> MigrationBuilder version currentSchema upgradable -> MigrationBuilder version mergedNubbedSchema upgradable
addTable tableName tableSchema (MigrationBuilder m) = MigrationBuilder $ updateSteps m
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
  Proxy tableName -> MigrationBuilder version currentSchema upgradable -> MigrationBuilder version newSchema upgradable
removeTable tableName (MigrationBuilder m) = MigrationBuilder $ updateSteps m
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
  Proxy tableName -> Proxy uniqueness -> Proxy index -> MigrationBuilder version currentDatabaseSchema upgradable -> MigrationBuilder version newDatabaseSchema upgradable
addIndex tableName _ _ (MigrationBuilder m) = MigrationBuilder $ updateSteps m
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
  Proxy tableName -> Proxy index -> MigrationBuilder version currentDatabaseSchema upgradable -> MigrationBuilder version newDatabaseSchema upgradable
removeIndex tableName _ (MigrationBuilder m) = MigrationBuilder $ updateSteps m
  where
    updateSteps = Record.modify _steps_ $ MigrationSteps.mapHead $ Record.modify _stores_ updateStores
    updateStores = Object.insert (reflectSymbol tableName) newTableSchema
    newTableSchema = Just $ serializeTableSchema (Proxy :: Proxy newTableSchema)

setUpgrade :: forall version databaseSchema.
  DatabaseSchema databaseSchema =>
  Shannon databaseSchema Unit -> MigrationBuilder version databaseSchema CanUpgrade -> MigrationBuilder version databaseSchema AlreadyHasUpgrade
setUpgrade _ (MigrationBuilder m) = MigrationBuilder $ updateSteps m
  where
    updateSteps = Record.modify _steps_ $ MigrationSteps.mapHead $ Record.set _upgrade_ newUpgrade
    newUpgrade = Just $ Promise.resolve unit -- TODO

-- | Used to remove type information only needed whilst defining migration
completeMigrationDefinition :: forall version databaseSchema upgradable.
  IsNat version =>
  DatabaseSchema databaseSchema =>
  MigrationBuilder version databaseSchema upgradable -> DefinedMigration databaseSchema
completeMigrationDefinition (MigrationBuilder m) = DefinedMigration m
