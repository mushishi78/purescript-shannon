module Shannon.Data.Migration where

import Shannon.Data.DatabaseSchema (class DatabaseSchema)
import Shannon.Data.MigrationSteps (MigrationSteps)
import Type.Data.Peano (class IsNat, Nat)

newtype Migration :: forall k. Nat -> Row k -> Type
newtype Migration version databaseSchema = Migration (
  IsNat version =>
  DatabaseSchema databaseSchema =>
    MigrationRecord
  )

type MigrationRecord = { dbName :: String, steps :: MigrationSteps }

getDBName ::
  forall version databaseSchema.
  IsNat version =>
  DatabaseSchema databaseSchema =>
  Migration version databaseSchema -> String
getDBName (Migration m) = m.dbName

getSteps ::
  forall version databaseSchema.
  IsNat version =>
  DatabaseSchema databaseSchema =>
  Migration version databaseSchema -> MigrationSteps
getSteps (Migration m) = m.steps
