module Shannon.Migrate where

import Prelude

import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty)
import Data.NonEmpty (singleton) as NonEmpty
import Dexie.Promise (Promise)
import Foreign.Object (Object)
import Foreign.Object (empty) as Object
import Prim.Ordering (LT)
import Shannon.Data (class DatabaseSchema)
import Type.Data.Peano.Nat (class CompareNat, class IsNat, D0, Nat)

type MigrationStep =
  { version :: Int
  , stores :: Object String
  , upgrade :: Maybe (Promise Unit)
  }

data Migration :: forall k. Nat -> Row k -> Type
data Migration version databaseSchema = Migration (
  IsNat version =>
  DatabaseSchema databaseSchema =>
    { dbName :: String
    , steps :: NonEmpty Array MigrationStep
    }
  )


emptyMigrationStep :: MigrationStep
emptyMigrationStep =
  { version: 0
  , stores: Object.empty
  , upgrade: Nothing
  }

defineMigration :: String -> Migration D0 ()
defineMigration dbName = Migration { dbName, steps: NonEmpty.singleton emptyMigrationStep }

newVersion ::
  forall v1 v2 databaseSchema proxy.
  IsNat v1 =>
  CompareNat v1 v2 LT =>
  proxy v2 -> Migration v1 databaseSchema -> Migration v2 databaseSchema
newVersion _ (Migration r) = Migration r
