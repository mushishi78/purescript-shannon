module Shannon.Data.Migration where

import Shannon.Data.DatabaseSchema (class DatabaseSchema)
import Shannon.Data.MigrationSteps (MigrationSteps)
import Type.Data.Peano (class IsNat, Nat)

newtype Migration :: forall k. Nat -> Row k -> Upgradable_ -> Type
newtype Migration version databaseSchema upgradable = Migration (
  IsNat version =>
  DatabaseSchema databaseSchema =>
    MigrationRecord
  )

newtype DefinedMigration :: forall k. Row k -> Type
newtype DefinedMigration databaseSchema = DefinedMigration (
  DatabaseSchema databaseSchema => MigrationRecord
)

type MigrationRecord = { dbName :: String, steps :: MigrationSteps }

data Upgradable_
foreign import data CannotUpgradeInitially :: Upgradable_
foreign import data CanUpgrade :: Upgradable_
foreign import data AlreadyHasUpgrade :: Upgradable_

getDBName ::
  forall version databaseSchema upgradable.
  IsNat version =>
  DatabaseSchema databaseSchema =>
  Migration version databaseSchema upgradable -> String
getDBName (Migration m) = m.dbName

getSteps ::
  forall version databaseSchema upgradable.
  IsNat version =>
  DatabaseSchema databaseSchema =>
  Migration version databaseSchema upgradable -> MigrationSteps
getSteps (Migration m) = m.steps
