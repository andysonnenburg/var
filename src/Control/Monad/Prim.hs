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

import Control.Applicative
import Control.Monad.Prim.Class

import GHC.Exts

newtype Prim s a = Prim { unPrim :: State# s -> (# State# s, a #) }

runPrim :: MonadPrim m => Prim (World m) a -> m a
runPrim = prim . unPrim
{-# INLINE runPrim #-}

instance Functor (Prim s) where
  fmap f m = Prim $ \ s -> case unPrim m s of
    (# s', a #) -> (# s', f a #)
  {-# INLINE fmap #-}

instance Applicative (Prim s) where
  pure a = Prim $ \ s -> (# s, a #)
  {-# INLINE pure #-}
  f <*> a = Prim $ \ s -> case unPrim f s of
    (# s', f' #) -> case unPrim a s' of
      (# s'', a' #) -> (# s'', f' a' #)
  {-# INLINE (<*>) #-}

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
