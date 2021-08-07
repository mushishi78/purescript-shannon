module Shannon.Data.Database where

import Prelude

import Shannon.Data.DatabaseSchema (class DatabaseSchema)

data Database :: forall k. Row k -> Type
data Database databaseSchema = Database
  ( DatabaseSchema databaseSchema =>
    { mappings :: Unit
    }
  )
