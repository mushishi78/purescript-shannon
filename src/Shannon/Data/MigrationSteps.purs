module Shannon.Data.MigrationSteps where

import Data.NonEmpty (NonEmpty, (:|))
import Data.Array ((:))
import Shannon.Data.MigrationStep (MigrationStep)

type MigrationSteps = NonEmpty Array MigrationStep

mapHead :: forall m a. (a -> a) -> NonEmpty m a -> NonEmpty m a
mapHead fn (head :| tail) = (fn head) :| tail

cons :: MigrationStep -> MigrationSteps -> MigrationSteps
cons newStep (head :| tail) = newStep :| head : tail
