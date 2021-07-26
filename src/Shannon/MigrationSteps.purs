module Shannon.MigrationSteps where

import Data.Array ((:))
import Data.NonEmpty (NonEmpty, (:|))
import Shannon.Data (MigrationStep, MigrationSteps)

mapHead :: forall m a. (a -> a) -> NonEmpty m a -> NonEmpty m a
mapHead fn (head :| tail) = (fn head) :| tail

cons :: MigrationStep -> MigrationSteps -> MigrationSteps
cons newStep (head :| tail) = newStep :| head : tail
