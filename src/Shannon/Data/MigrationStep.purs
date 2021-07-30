module Shannon.Data.MigrationStep where

import Prelude

import Data.Maybe (Maybe(..))
import Dexie.Promise (Promise)
import Foreign.Object (Object)
import Foreign.Object as Object

type MigrationStep =
  { version :: Int
  , stores :: Object (Maybe String)
  , upgrade :: Maybe (Promise Unit)
  }

empty :: Int -> MigrationStep
empty version =
  { version: version
  , stores: Object.empty
  , upgrade: Nothing
  }
