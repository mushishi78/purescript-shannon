module Shannon.Migrate where

import Prelude

import Prim.Ordering (LT)
import Shannon.Data (class DatabaseSchema, Database)
import Type.Data.Peano.Nat (Nat, class CompareNat)
import Type.Proxy (Proxy)

data Migration :: forall k. Nat -> Row k -> Type
data Migration (version :: Nat) schemaChanges = Migration (DatabaseSchema schemaChanges => Proxy schemaChanges)

migrate :: forall currentVersion dbSchema desiredVersion schemaChanges.
  CompareNat currentVersion desiredVersion LT =>
  Database currentVersion dbSchema -> Migration desiredVersion schemaChanges -> Unit
migrate _ _ = unit
