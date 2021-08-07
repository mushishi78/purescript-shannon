module Shannon.Symbol where

import Type.Proxy (Proxy(..))

_dbName_ = Proxy :: Proxy "dbName"
_steps_ = Proxy :: Proxy "steps"

_version_ = Proxy :: Proxy "version"
_stores_ = Proxy :: Proxy "stores"
_upgrade_ = Proxy :: Proxy "upgrade"
