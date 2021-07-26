module Shannon.Data.Proxy where

import Shannon.Data.TableSchema (CompoundIndex, InboundPrimaryKey, Incrementing, Index, NonIncrementing, NotUnique, OutboundPrimaryKey, Unique, WithIndex)
import Type.Proxy (Proxy(..))

outbound :: forall incr. Proxy incr -> Proxy (OutboundPrimaryKey incr)
outbound _ = Proxy

inbound :: forall incr indx. Proxy incr -> Proxy indx -> Proxy (InboundPrimaryKey incr indx)
inbound _ _ = Proxy

withIndex :: forall uniq indx tail. Proxy uniq -> Proxy indx -> Proxy tail -> Proxy (WithIndex uniq indx tail)
withIndex _ _ _ = Proxy

incrementing = Proxy :: Proxy Incrementing
nonIncrementing = Proxy :: Proxy NonIncrementing
unique = Proxy :: Proxy Unique
notUnique = Proxy :: Proxy NotUnique

index :: forall sym. Proxy sym -> Proxy (Index sym)
index _ = Proxy

compoundIndex :: forall sym indx. Proxy sym -> Proxy indx -> Proxy (CompoundIndex sym indx)
compoundIndex _ _ = Proxy
