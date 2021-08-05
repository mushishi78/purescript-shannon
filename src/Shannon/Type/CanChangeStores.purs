module Shannon.Type.CanChangeStores where

import Prim.TypeError (class Fail, Text)
import Shannon.Data.Migration (AlreadyHasUpgrade, CanUpgrade, CannotUpgradeInitially, Upgradable_)

-- | Can only make changes to the stores (add and remove tables or indexes)
-- | before the upgrade for the current version
class CanChangeStores :: Upgradable_ -> Constraint
class CanChangeStores upgradable

instance canChangeStores_CannotUpgradeInitially :: CanChangeStores CannotUpgradeInitially

instance canChangeStores_CanUpgrade :: CanChangeStores CanUpgrade

instance canChangeStores_Fail
    :: (Fail (Text "Cannot change store after upgrade in migration step"))
    => CanChangeStores AlreadyHasUpgrade