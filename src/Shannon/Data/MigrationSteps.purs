module Shannon.Data.MigrationSteps where

import Data.Array ((:))
import Data.NonEmpty (NonEmpty, (:|), singleton)
import Shannon.Data.MigrationStep (MigrationStep)
import Shannon.Data.MigrationStep as MigrationStep

type MigrationSteps = NonEmpty Array MigrationStep

mapHead :: forall m a. (a -> a) -> NonEmpty m a -> NonEmpty m a
mapHead fn (head :| tail) = (fn head) :| tail

cons :: MigrationStep -> MigrationSteps -> MigrationSteps
cons newStep (head :| tail) = newStep :| head : tail

empty :: MigrationSteps
empty = singleton (MigrationStep.empty 0)
