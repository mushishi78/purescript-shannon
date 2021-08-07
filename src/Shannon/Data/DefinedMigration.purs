module Shannon.Data.DefinedMigration where

import Shannon.Data.DatabaseSchema (class DatabaseSchema)
import Shannon.Data.MigrationSteps (MigrationSteps)

newtype DefinedMigration :: forall k. Row k -> Type
newtype DefinedMigration databaseSchema = DefinedMigration (
  DatabaseSchema databaseSchema => { dbName :: String, steps :: MigrationSteps }
)

getDBName ::
  forall databaseSchema.
  DatabaseSchema databaseSchema =>
  DefinedMigration databaseSchema -> String
getDBName (DefinedMigration m) = m.dbName

getSteps ::
  forall databaseSchema.
  DatabaseSchema databaseSchema =>
  DefinedMigration databaseSchema -> MigrationSteps
getSteps (DefinedMigration m) = m.steps
