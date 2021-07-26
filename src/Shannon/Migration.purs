module Shannon.Migration where

import Prelude

import Data.NonEmpty (singleton) as NonEmpty
import Data.Symbol (class IsSymbol)
import Prim.Ordering (LT)
import Prim.Row (class Cons, class Lacks)
import Record as Record
import Shannon.Data (class DatabaseSchema, TableSchema_, Migration(..))
import Shannon.MigrationStep as MigrationStep
import Shannon.MigrationSteps as MigrationSteps
import Shannon.Symbol (_steps_)
import Shannon.Type.SerializeSchema (class SerializeTableSchema)
import Type.Data.Peano.Nat (class CompareNat, class IsNat, D0, reflectNat)
import Type.Proxy (Proxy(..))

defineMigration :: String -> Migration D0 ()
defineMigration dbName = Migration { dbName, steps: NonEmpty.singleton (MigrationStep.empty 0) }

newVersion ::
  forall v1 v2 databaseSchema proxy.
  IsNat v1 =>
  IsNat v2 =>
  CompareNat v1 v2 LT =>
  proxy v2 -> Migration v1 databaseSchema -> Migration v2 databaseSchema
newVersion _ (Migration m) = Migration $ Record.modify _steps_ (MigrationSteps.cons newStep) m
  where
    newStep = MigrationStep.empty $ reflectNat $ (Proxy :: Proxy v2)

addTable ::
  forall version currentSchema tableName (tableSchema :: TableSchema_) mergedSchema.
  IsSymbol tableName =>
  Lacks tableName currentSchema =>
  Cons tableName tableSchema currentSchema mergedSchema =>
  DatabaseSchema currentSchema =>
  SerializeTableSchema tableSchema =>
  Proxy tableName -> Proxy tableSchema -> Migration version currentSchema -> Migration version mergedSchema
addTable tableName tableSchema (Migration m) = Migration $ updateSteps m
  where
    updateSteps = Record.modify _steps_ $ MigrationSteps.mapHead $ MigrationStep.addToStores tableName tableSchema
