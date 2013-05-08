module Data.Proxy
       ( Proxy (..)
       , proxy
       , reproxy
       , reproxyRep
       ) where

import GHC.Generics

data Proxy a = Proxy

proxy :: a -> Proxy a
proxy _ = Proxy
{-# INLINE proxy #-}

reproxy :: t a -> Proxy b
reproxy _ = Proxy
{-# INLINE reproxy #-}

reproxyRep :: t a -> Proxy (Rep a p)
reproxyRep _ = Proxy
{-# INLINE reproxyRep #-}
