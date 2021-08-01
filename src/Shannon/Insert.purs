module Shannon.Insert where

import Prelude

import Control.Monad.Reader (ReaderT(..))
import Data.Symbol (class IsSymbol)
import Dexie.Promise as Promise
import Shannon.Data.Shannon (Shannon)
import Shannon.Type.InsertKey (class InsertKeyInDatabaseSchema)
import Type.Proxy (Proxy)

insert ::
  forall dbSchema tableName insertKey insertRow.
    IsSymbol tableName =>
    InsertKeyInDatabaseSchema dbSchema tableName insertKey =>
    Proxy tableName -> insertKey -> Record insertRow -> Shannon dbSchema Unit
insert _ _ _ = ReaderT \_ -> Promise.resolve unit
