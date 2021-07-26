module Shannon.MigrationStep where

import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Foreign.Object (Object)
import Foreign.Object (empty, insert) as Object
import Record as Record
import Shannon.Data (MigrationStep)
import Shannon.Symbol (_stores_)
import Shannon.Type.SerializeSchema (class SerializeTableSchema, serializeTableSchema)
import Type.Proxy (Proxy(..))

empty :: Int -> MigrationStep
empty version =
  { version: version
  , stores: Object.empty
  , upgrade: Nothing
  }

addToStores ::
  forall tableName tableSchema.
  IsSymbol tableName =>
  SerializeTableSchema tableSchema =>
  Proxy tableName -> Proxy tableSchema -> MigrationStep -> MigrationStep
addToStores _ _ = Record.modify _stores_ updateStores
  where
    updateStores :: Object String -> Object String
    updateStores = Object.insert
        (reflectSymbol (Proxy :: Proxy tableName))
        (serializeTableSchema (Proxy :: Proxy tableSchema))
