module Shannon.Data.MigrationDefinition where

import Shannon.Data.DatabaseSchema (class DatabaseSchema)
import Shannon.Data.MigrationSteps (MigrationSteps)

newtype MigrationDefinition :: forall k. Row k -> Type
newtype MigrationDefinition databaseSchema = MigrationDefinition
  ( DatabaseSchema databaseSchema => { dbName :: String, steps :: MigrationSteps }
  )

getDBName ::
  forall databaseSchema.
  DatabaseSchema databaseSchema =>
  MigrationDefinition databaseSchema ->
  String
getDBName (MigrationDefinition m) = m.dbName

getSteps ::
  forall databaseSchema.
  DatabaseSchema databaseSchema =>
  MigrationDefinition databaseSchema ->
  MigrationSteps
getSteps (MigrationDefinition m) = m.steps
