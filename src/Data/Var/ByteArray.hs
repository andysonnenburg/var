{-# LANGUAGE
    CPP
  , DeriveDataTypeable
  , FlexibleInstances
  , MultiParamTypeClasses #-}
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
import Data.Prim.ByteArray
import Data.Var.Class
import Data.Typeable (Typeable)

newtype ByteArrayVar s a = ByteArrayVar (MutableByteArray s) deriving (Eq, Typeable)

instance (ByteArraySlice a, MonadPrim m, s ~ World m) => Var (ByteArrayVar s) a m where
  newVar a = do
    array <- newByteArray (byteSizeOf a)
    writeByteOff array 0 a
    return $ ByteArrayVar array
  {-# INLINE newVar #-}
  readVar (ByteArrayVar array) = readByteOff array 0
  {-# INLINE readVar #-}
  writeVar (ByteArrayVar array) = writeByteOff array 0
  {-# INLINE writeVar #-}
