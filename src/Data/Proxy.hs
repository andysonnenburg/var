{-# LANGUAGE TypeOperators #-}
module Data.Proxy
       ( Proxy (..)
       , proxy
       , reproxy
       , reproxyRep
       , reproxyK1
       , reproxyM1
       , reproxyFst
       , reproxySnd
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
reproxyRep = reproxy
{-# INLINE reproxyRep #-}

reproxyK1 :: t (K1 i c p) -> Proxy c
reproxyK1 = reproxy
{-# INLINE reproxyK1 #-}

reproxyM1 :: t (M1 i c f p) -> Proxy (f p)
reproxyM1 = reproxy
{-# INLINE reproxyM1 #-}

reproxyFst :: t ((f :*: g) p) -> Proxy (f p)
reproxyFst = reproxy
{-# INLINE reproxyFst #-}

reproxySnd :: t ((f :*: g) p) -> Proxy (g p)
reproxySnd = reproxy
{-# INLINE reproxySnd #-}
