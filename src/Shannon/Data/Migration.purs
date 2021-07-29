module Shannon.Data.Migration where

import Shannon.Data.DatabaseSchema (class DatabaseSchema)
import Shannon.Data.MigrationSteps (MigrationSteps)
import Type.Data.Peano (class IsNat, Nat)

newtype Migration :: forall k. Nat -> Row k -> Type
newtype Migration version databaseSchema = Migration (
  IsNat version =>
  DatabaseSchema databaseSchema =>
    { dbName :: String
    , steps :: MigrationSteps
    }
  )

