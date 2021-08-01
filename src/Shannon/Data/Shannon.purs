module Shannon.Data.Shannon where

import Prelude
import Control.Monad.Reader (ReaderT, runReaderT)
import Dexie.Promise (Promise)
import Dexie.Promise as Promise
import Effect.Aff (Aff)
import Shannon.Data.Database (Database)

{-| Monad stack for running queries under
    Has reader access to database and causes effects in a dexie promise
-}
type Shannon :: forall k. Row k -> Type -> Type
type Shannon dbSchema value = ReaderT (Database dbSchema) Promise value

withImplicitTransactions :: forall dbSchema v. Database dbSchema -> Shannon dbSchema v -> Aff v
withImplicitTransactions db effect = Promise.toAff $ runReaderT effect db
