module Data.Proxy
       ( Proxy (..)
       , proxy
       , reproxy
       ) where

data Proxy a = Proxy

proxy :: a -> Proxy a
proxy _ = Proxy
{-# INLINE proxy #-}

reproxy :: t a -> Proxy b
reproxy _ = Proxy
{-# INLINE reproxy #-}
