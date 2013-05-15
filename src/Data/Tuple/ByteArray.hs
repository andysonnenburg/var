{-# LANGUAGE CPP #-}
#ifdef LANGUAGE_DataKinds
{-# LANGUAGE DataKinds #-}
#endif
{-# LANGUAGE DefaultSignatures, DeriveDataTypeable #-}
#ifndef LANGUAGE_DataKinds
{-# LANGUAGE EmptyDataDecls #-}
#endif
{-# LANGUAGE
    FlexibleInstances
  , FlexibleContexts
  , MagicHash
  , MultiParamTypeClasses #-}
#ifdef LANGUAGE_Trustworthy
{-# LANGUAGE Trustworthy #-}
#endif
{-# LANGUAGE
    TypeFamilies
  , UnboxedTuples
  , UndecidableInstances #-}
{- |
Copyright   :  (c) Andy Sonnenburg 2013
License     :  BSD3
Maintainer  :  andy22286@gmail.com
-}
module Data.Tuple.ByteArray
       ( ByteArrayTuple
       ) where

import Control.Monad.Prim.Class

import Data.ByteArraySlice.Unsafe
import Data.Proxy
import Data.Tuple.Class
import Data.Tuple.MTuple
import Data.Typeable (Typeable)

import GHC.Exts

data ByteArrayTuple s a = ByteArrayTuple (MutableByteArray# s) deriving Typeable

instance Eq (ByteArrayTuple s a) where
  ByteArrayTuple a == ByteArrayTuple b = sameMutableByteArray# a b

instance ( MonadPrim m
         , s ~ World m
         , Tuple t
         , ByteArraySlice t
         ) => MTuple (ByteArrayTuple s) t m where
  thawTuple a = liftPrim $ \ s -> case newByteArray# (byteSizeOf# a) s of
    (# s', array #) -> case writeByteArray# array 0# a s' of
      s'' -> (# s'', ByteArrayTuple array #)
  freezeTuple (ByteArrayTuple array) = liftPrim $ readByteArray# array 0#

instance ( MonadPrim m
         , s ~ World m
         , Tuple t
         , ByteArraySlice t
         , a ~ Field1 t
         , ByteArraySlice a
         ) => MField1 (ByteArrayTuple s) t a m where
  read1 = unsafeRead 0#
  write1 = unsafeWrite 0#

instance ( MonadPrim m
         , s ~ World m
         , Tuple t
         , ByteArraySlice t
         , ByteArraySlice (Field1 t)
         , a ~ Field2 t
         , ByteArraySlice a
         ) => MField2 (ByteArrayTuple s) t a m where
  read2 t = unsafeRead (byteSize# (reproxyField1 t)) t
  write2 t = unsafeWrite (byteSize# (reproxyField1 t)) t

instance ( MonadPrim m
         , s ~ World m
         , Tuple t
         , ByteArraySlice t
         , ByteArraySlice (Field1 t)
         , ByteArraySlice (Field2 t)
         , a ~ Field3 t
         , ByteArraySlice a
         ) => MField3 (ByteArrayTuple s) t a m where
  read3 t = unsafeRead (byteSize# (reproxyField1 t) +#
                        byteSize# (reproxyField2 t)) t
  write3 t = unsafeWrite (byteSize# (reproxyField1 t) +#
                          byteSize# (reproxyField2 t)) t

instance ( MonadPrim m
         , s ~ World m
         , Tuple t
         , ByteArraySlice t
         , ByteArraySlice (Field1 t)
         , ByteArraySlice (Field2 t)
         , ByteArraySlice (Field3 t)
         , a ~ Field4 t
         , ByteArraySlice a
         ) => MField4 (ByteArrayTuple s) t a m where
  read4 t = unsafeRead (byteSize# (reproxyField1 t) +#
                        byteSize# (reproxyField2 t) +#
                        byteSize# (reproxyField3 t)) t
  write4 t = unsafeWrite (byteSize# (reproxyField1 t) +#
                          byteSize# (reproxyField2 t) +#
                          byteSize# (reproxyField3 t)) t

reproxyField1 :: t a -> Proxy (Field1 a)
reproxyField1 = reproxy

reproxyField2 :: t a -> Proxy (Field2 a)
reproxyField2 = reproxy

reproxyField3 :: t a -> Proxy (Field3 a)
reproxyField3 = reproxy

unsafeRead :: ( ByteArraySlice a
              , MonadPrim m
              ) => Int# -> ByteArrayTuple (World m) t -> m a
unsafeRead i (ByteArrayTuple array) = liftPrim $ readByteArray# array i

unsafeWrite :: ( ByteArraySlice a
               , MonadPrim m
               ) => Int# -> ByteArrayTuple (World m) t -> a -> m ()
unsafeWrite i (ByteArrayTuple array) a = liftPrim $ \ s ->
  case writeByteArray# array i a s of
    s' -> (# s', () #)
