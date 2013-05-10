{-# LANGUAGE TypeOperators #-}
module Data.Proxy
       ( Proxy (..)
       , proxy
       , reproxy
       , reproxyRep
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
reproxyRep _ = Proxy
{-# INLINE reproxyRep #-}

reproxyM1 :: t (M1 i c f p) -> Proxy (f p)
reproxyM1 _ = Proxy
{-# INLINE reproxyM1 #-}

reproxyFst :: t ((f :*: g) p) -> Proxy (f p)
reproxyFst _ = Proxy
{-# INLINE reproxyFst #-}

reproxySnd :: t ((f :*: g) p) -> Proxy (g p)
reproxySnd _ = Proxy
{-# INLINE reproxySnd #-}
