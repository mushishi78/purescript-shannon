module Shannon.Data.Migration where

import Shannon.Data.DatabaseSchema (class DatabaseSchema)
import Shannon.Data.MigrationSteps (MigrationSteps)
import Type.Data.Peano (class IsNat, Nat)

data Migration :: forall k. Nat -> Row k -> Type
data Migration version databaseSchema = Migration (
  IsNat version =>
  DatabaseSchema databaseSchema =>
    { dbName :: String
    , steps :: MigrationSteps
    }
  )
