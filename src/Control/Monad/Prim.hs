{-# LANGUAGE MagicHash, TypeFamilies, UnboxedTuples #-}
{- |
Copyright   :  (c) Andy Sonnenburg 2013
License     :  BSD3
Maintainer  :  andy22286@gmail.com
-}
module Control.Monad.Prim
       ( module Control.Monad.Prim.Class
       , Prim
       , runPrim
       ) where

import Control.Monad.Prim.Class

import GHC.Exts

newtype Prim s a = Prim { unPrim :: State# s -> (# State# s, a #) }

runPrim :: MonadPrim m => Prim (World m) a -> m a
runPrim = prim . unPrim
{-# INLINE runPrim #-}

instance Monad (Prim s) where
  return a = Prim $ \ s -> (# s, a #)
  {-# INLINE return #-}
  m >>= k = Prim $ \ s -> case unPrim m s of
    (# s', a #) -> unPrim (k a) s'
  {-# INLINE (>>=) #-}
  m >> n = Prim $ \ s -> case unPrim m s of
    (# s', _ #) -> unPrim n s'
  {-# INLINE (>>) #-}

instance MonadPrim (Prim s) where
  type World (Prim s) = s
  prim = Prim
  {-# INLINE prim #-}
