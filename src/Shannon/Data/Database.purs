module Shannon.Data.Database where

import Data.Maybe (Maybe)
import Dexie.Data (DB, Transaction)
import Shannon.Data.DatabaseSchema (class DatabaseSchema)

data Database :: forall k. Row k -> Type
data Database databaseSchema = Database
  ( DatabaseSchema databaseSchema =>
    { db :: DB
    , trnx :: Maybe Transaction
    }
  )
