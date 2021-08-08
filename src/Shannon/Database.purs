module Shannon.Database where

import Prelude

import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Dexie as Dexie
import Dexie.DB as DB
import Dexie.Version as Version
import Effect.Class (class MonadEffect)
import Foreign.Object as Object
import Shannon.Data.Database (Database(..))
import Shannon.Data.DatabaseSchema (class DatabaseSchema)
import Shannon.Data.MigrationDefinition (MigrationDefinition, getDBName, getSteps)

initializeDatabase ::
  forall databaseSchema monadEffect.
  DatabaseSchema databaseSchema =>
  MonadEffect monadEffect =>
  MigrationDefinition databaseSchema ->
  monadEffect (Database databaseSchema)
initializeDatabase migrationDef = do
  db <- Dexie.new (getDBName migrationDef)

  for_ (getSteps migrationDef) $ \step -> do
    version <- DB.version (step.version) db

    -- Run schema change if present
    if Object.isEmpty step.stores then pure unit
    else Version.stores_ step.stores version

    -- Run data change if present
    case step.upgrade of
      Nothing -> pure unit
      Just upgrade -> Version.upgrade_ (upgrade db) version

  pure $ (Database { db: db, trnx: Nothing })
