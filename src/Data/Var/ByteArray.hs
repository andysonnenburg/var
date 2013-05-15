{-# LANGUAGE
    CPP
  , DeriveDataTypeable
  , FlexibleInstances
  , MagicHash
  , MultiParamTypeClasses
  , UnboxedTuples #-}
#ifdef LANGUAGE_Trustworthy
{-# LANGUAGE Trustworthy #-}
#endif
{-# LANGUAGE TypeFamilies #-}
{- |
Copyright   :  (c) Andy Sonnenburg 2013
License     :  BSD3
Maintainer  :  andy22286@gmail.com
-}
module Data.Var.ByteArray
       ( ByteArrayVar
       ) where

import Control.Monad.Prim.Class

import Data.ByteArraySlice.Unsafe
import Data.Var.Class
import Data.Typeable (Typeable)

import GHC.Exts

data ByteArrayVar s a = ByteArrayVar (MutableByteArray# s) deriving Typeable

instance Eq (ByteArrayVar s a) where
  ByteArrayVar a == ByteArrayVar b = sameMutableByteArray# a b

instance (ByteArraySlice a, MonadPrim m, s ~ World m) => Var (ByteArrayVar s) a m where
  newVar a = liftPrim $ \ s -> case newByteArray# (byteSizeOf# a) s of
    (# s', array #) -> case writeByteArray# array 0# a s' of
      s'' -> (# s'', ByteArrayVar array #)
  {-# INLINE newVar #-}
  readVar (ByteArrayVar array) = liftPrim $ readByteArray# array 0#
  {-# INLINE readVar #-}
  writeVar (ByteArrayVar array) a = liftPrim $ \ s ->
    case writeByteArray# array 0# a s of
      s' -> (# s', () #)
  {-# INLINE writeVar #-}
