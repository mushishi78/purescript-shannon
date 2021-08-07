module Shannon.Data.MigrationBuilder where

import Shannon.Data.DatabaseSchema (class DatabaseSchema)
import Shannon.Data.MigrationSteps (MigrationSteps)
import Type.Data.Peano (class IsNat, Nat)

newtype MigrationBuilder :: forall k. Nat -> Row k -> Upgradable_ -> Type
newtype MigrationBuilder version databaseSchema upgradable = MigrationBuilder
  ( IsNat version =>
    DatabaseSchema databaseSchema =>
    { dbName :: String, steps :: MigrationSteps }
  )

data Upgradable_
foreign import data CannotUpgradeInitially :: Upgradable_
foreign import data CanUpgrade :: Upgradable_
foreign import data AlreadyHasUpgrade :: Upgradable_

getDBName ::
  forall version databaseSchema upgradable.
  IsNat version =>
  DatabaseSchema databaseSchema =>
  MigrationBuilder version databaseSchema upgradable ->
  String
getDBName (MigrationBuilder m) = m.dbName

getSteps ::
  forall version databaseSchema upgradable.
  IsNat version =>
  DatabaseSchema databaseSchema =>
  MigrationBuilder version databaseSchema upgradable ->
  MigrationSteps
getSteps (MigrationBuilder m) = m.steps
