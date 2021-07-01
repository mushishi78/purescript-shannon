module Shannon where

import Data.Symbol (SProxy)
import Foreign.Index (class Index)
import Type.Function (FLIP)
import Type.Proxy (Proxy(..))

infixl 0 type FLIP as #

data IndexSchema_
foreign import data OutboundPrimaryKey :: Incrementing_ -> IndexSchema_
foreign import data InboundPrimaryKey :: Incrementing_ -> Index_ -> IndexSchema_
foreign import data WithIndex :: Uniqueness_ -> Index_ -> IndexSchema_ -> IndexSchema_

data Incrementing_
foreign import data Incrementing :: Incrementing_
foreign import data NonIncrementing :: Incrementing_

data Uniqueness_
foreign import data Unique :: Uniqueness_
foreign import data NotUnique :: Uniqueness_

data Index_
foreign import data Index :: Symbol -> Index_
foreign import data CompoundIndex :: Index_ -> Symbol -> Index_
infixl 0 type CompoundIndex as <+>

type Foo =
    OutboundPrimaryKey Incrementing
        # WithIndex NotUnique (Index "age")
        # WithIndex NotUnique (Index "id" <+> "age" <+> "name")
